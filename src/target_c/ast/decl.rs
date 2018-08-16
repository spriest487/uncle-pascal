use std::fmt;
use target_c::ast::{
    Name,
    CType,
    FunctionDecl,
    TranslationResult,
    TranslationUnit,
    Expression,
    CallingConvention,
    FunctionDefinition,
    FunctionArg,
    Block,
    CastKind,
};
use semantic;
use node::{
    self,
    UnitDecl,
    TypeDecl,
    RecordKind,
};

#[derive(Clone, Debug, PartialEq)]
pub struct Variable {
    pub name: Name,
    pub ctype: CType,
    pub array_size: Option<usize>,
    pub default_value: Option<Expression>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum StructMember {
    Field(Variable),
    AnonymousUnion(Vec<StructMember>),
    Struct(Box<Struct>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Struct {
    pub name: Option<Name>,
    pub extends: Option<Name>,
    pub members: Vec<StructMember>,
}

pub enum Declaration {
    Variable(Variable),
    Function(FunctionDecl),
    Struct(Struct),
    UsingAlias(Name, CType),
}

impl Declaration {
    pub fn translate_decl(decl: &semantic::UnitDecl,
                          unit: &mut TranslationUnit)
                          -> TranslationResult<Vec<Declaration>> {
        match decl {
            UnitDecl::Function(func_decl) => {
                let function = FunctionDecl::from(func_decl);
                Ok(vec!(Declaration::Function(function)))
            }

            UnitDecl::Type(ref type_decl) =>
                match type_decl {
                    | TypeDecl::Record(record_decl) => {
                        let (struct_decl, struct_ctor) = Struct::translate(record_decl, unit)?;
                        let results = vec![
                            Declaration::Struct(struct_decl),
                            Declaration::Function(struct_ctor),
                        ];

                        Ok(results)
                    }

                    | TypeDecl::Enumeration(enum_decl) => {
                        let name = Name::internal_type(&enum_decl.qualified_name());
                        Ok(vec!(Declaration::UsingAlias(
                            name,
                            CType::Named(Name::internal_type("UInt64")),
                        )))
                    }

                    | TypeDecl::Set(set_decl) => {
                        //todo
                        let name = Name::user_type(&set_decl.qualified_name());
                        Ok(vec!(Declaration::UsingAlias(
                            name,
                            CType::Named(Name::internal_type("Set<System_UInt64>".to_string()),
                            ))))
                    }

                    | TypeDecl::Alias { .. } => {
                        //should be removed during typechecking, can be ignored here
                        Ok(vec![])
                    }

                    | TypeDecl::Interface(_) => {
                        // we don't store any information about interfaces in the C++ output yet
                        Ok(vec![])
                    }
                }

            UnitDecl::Var(ref var_decl) => {
                let variable = Variable::translate(var_decl, false, unit)?;
                Ok(vec!(Declaration::Variable(variable)))
            }

            UnitDecl::Const(_) => {
                // consts are resolved at compile-time and don't need to be in the C++ output
                Ok(vec![])
            }
        }
    }

    pub fn translate_impl(decl: &semantic::Implementation,
                          unit: &mut TranslationUnit) ->
                          TranslationResult<Vec<Declaration>> {
        match decl {
            node::Implementation::Function(func_impl) => {
                let definition = FunctionDecl::translate(func_impl, unit)?;

                let result = match FunctionDecl::virtual_call_adaptor(&func_impl.decl) {
                    Some(virtual_call_adaptor) => vec![
                        Declaration::Function(definition),
                        Declaration::Function(virtual_call_adaptor)
                    ],

                    None => vec!(Declaration::Function(definition)),
                };

                Ok(result)
            }

            node::Implementation::Decl(decl) => {
                Self::translate_decl(decl, unit)
            }
        }
    }

    pub fn write_forward(&self, mut out: impl fmt::Write) -> fmt::Result {
        match self {
            Declaration::Variable(variable) => {
                variable.write_forward(&mut out)
            }

            Declaration::Function(function) =>
                function.write_forward(&mut out),

            Declaration::Struct(record) =>
                record.write_forward(&mut out),

            Declaration::UsingAlias(alias, target) =>
                writeln!(out, "using {} = {};", alias, target)
        }
    }

    pub fn write_impl(&self, mut out: impl fmt::Write) -> fmt::Result {
        match self {
            Declaration::Variable(variable) => {
                variable.write_impl(&mut out)
            }

            Declaration::Function(function) =>
                function.write_impl(&mut out),

            Declaration::Struct(record) =>
                record.write_def(&mut out),

            Declaration::UsingAlias(_alias, _target) =>
                Ok(())
        }
    }
}

impl Struct {
    pub fn translate(record_decl: &semantic::RecordDecl,
                     _unit: &mut TranslationUnit)
                     -> TranslationResult<(Struct, FunctionDecl)> {
        assert!(record_decl.members.len() > 0 || record_decl.variant_part.is_some(),
                "structs must have at least one member");

        let name = Name::user_type(&record_decl.scope().namespace_qualify(&record_decl.name));

        let mut members: Vec<_> = record_decl.members.iter()
            .map(|member_decl| {
                let ctype = CType::translate(&member_decl.decl_type, record_decl.scope());

                Ok(StructMember::Field(Variable {
                    name: Name::member(&member_decl.name),
                    default_value: None,
                    array_size: None,
                    ctype,
                }))
            })
            .collect::<TranslationResult<_>>()?;

        if let Some(variant_part) = &record_decl.variant_part {
            let tag = Variable {
                name: Name::member(&variant_part.tag.name),
                ctype: CType::translate(&variant_part.tag.decl_type, record_decl.scope()),
                default_value: None,
                array_size: None,
            };
            members.push(StructMember::Field(tag));

            let cases = variant_part.cases.iter()
                .map(|case| {
                    /* each case is an anonymous struct inside the union, containing all the
                    membes of that variant */
                    let case_struct_members: Vec<_> = case.members.iter()
                        .map(|member| {
                            StructMember::Field(Variable {
                                name: Name::member(&member.name),
                                ctype: CType::translate(&member.decl_type, record_decl.scope()),
                                default_value: None,
                                array_size: None,
                            })
                        })
                        .collect();

                    let anon_struct = Struct {
                        members: case_struct_members,
                        name: None,
                        extends: None,
                    };

                    Ok(StructMember::Struct(Box::new(anon_struct)))
                })
                .collect::<TranslationResult<_>>()?;

            members.push(StructMember::AnonymousUnion(cases))
        }

        let extends = match record_decl.kind {
            RecordKind::Record => None,
            RecordKind::Class => Some(Name::internal_type("Object")),
        };

        let constructor_decl = Struct::constructor_decl(&name, &record_decl);

        let struct_decl = Struct {
            name: Some(name),
            members,
            extends,
        };

        Ok((struct_decl, constructor_decl))
    }

    pub fn write_forward(&self, mut out: impl fmt::Write) -> fmt::Result {
        if let Some(name) = &self.name {
            writeln!(out, "struct {};", name)?;
        }
        Ok(())
    }

    pub fn write_def(&self, out: &mut fmt::Write) -> fmt::Result {
        write!(out, "struct ")?;
        if let Some(name) = &self.name {
            write!(out, "{} ", name)?;
        }

        if let Some(extends) = &self.extends {
            write!(out, " : {}", extends)?;
        }
        writeln!(out, "{{")?;

        for member in self.members.iter() {
            member.write(out)?;
        }

        writeln!(out, "}};")
    }

    // todo: this should probably be based on semantic::RecordDecl instead
    fn constructor_decl(struct_name: &Name, record: &semantic::RecordDecl) -> FunctionDecl {
        let name = Name::constructor(&record.qualified_name());
        let result_name = Name::local("result");

        let all_fields: Vec<_> = record.all_members().collect();

        /* the args of a constructor are Options of each individual member */
        let args: Vec<_> = all_fields.iter().enumerate()
            .map(|(mem_num, member)| FunctionArg {
                name: Name::local(format!("arg{}", mem_num)),
                ctype: {
                    let member_type = CType::translate(&member.decl_type, record.scope());
                    CType::Named(Name::internal_type(format!("Option<{}>", member_type)))
                },
            })
            .collect();

        let struct_type = CType::Struct(struct_name.clone());
        let (return_type, result_val) = match record.kind {
            RecordKind::Class => {
                let deref_result = Expression::Name(result_name.clone()).deref();
                (struct_type.into_pointer(), deref_result)
            }
            RecordKind::Record => {
                (struct_type, Expression::Name(result_name.clone()))
            }
        };

        let mut statements = Vec::new();
        statements.push(Expression::local_decl(return_type.clone(), "result", None));
        match record.kind {
            /* rc-allocate new class instance */
            RecordKind::Class => {
                statements.push(Expression::binary_op(
                    result_name.clone(), "=",
                    Expression::cast(
                        return_type.clone(),
                        Expression::function_call(Name::internal_symbol("Rc_GetMem"), vec![
                            Expression::function_call(Name::internal_symbol("SizeOf"), vec![
                                Expression::Name(struct_name.clone())
                            ]),
                            Expression::string_literal(&record.qualified_name().to_string())
                        ]),
                        CastKind::Static,
                    ),
                ));
            }

            /* zero-initialize a new record on the stack */
            RecordKind::Record => {
                statements.push(Expression::function_call(
                    Name::internal_symbol("ZeroMemory"),
                    vec![
                        Expression::unary_op("&", result_name.clone(), true),
                        Expression::function_call(Name::internal_symbol("SizeOf"), vec![
                            Expression::Name(result_name.clone())
                        ])
                    ],
                ));
            }
        }

        for (mem_num, member) in all_fields.iter().enumerate() {
            let result_member = Expression::member(result_val.clone(), &member.name);
            let arg_name = Name::local(format!("arg{}", mem_num));

            let deref_arg = Expression::unary_op("*", arg_name.clone(), true);
            let assign_member = Expression::binary_op(result_member, "=", deref_arg);

            let assign_if_present = Expression::if_then(arg_name, assign_member);

            statements.push(assign_if_present);
        }

        statements.push(Expression::return_value(result_name));

        let definition = FunctionDefinition::Defined(Block::new(statements));

        FunctionDecl {
            name,
            args,
            return_type,
            calling_convention: CallingConvention::Cdecl,
            definition,
        }
    }
}

impl StructMember {
    pub fn write(&self, out: &mut fmt::Write) -> fmt::Result {
        match self {
            StructMember::Field(field) => {
                writeln!(out, "{} {};", field.ctype, field.name)?;
            }

            StructMember::Struct(nested_struct) => {
                nested_struct.write_def(out)?;
            }

            StructMember::AnonymousUnion(cases) => {
                writeln!(out, "union {{")?;
                for case in cases.iter() {
                    case.write(out)?;
                }
                writeln!(out, "}};")?;
            }
        }

        Ok(())
    }
}

impl Variable {
    pub fn translate(var_decl: &semantic::VarDecl,
                     local: bool,
                     unit: &mut TranslationUnit)
                     -> TranslationResult<Variable> {
        let name = match local {
            false => {
                let full_id = var_decl.scope().namespace_qualify(&var_decl.name);
                Name::user_symbol(&full_id)
            }
            true => {
                Name::local(&var_decl.name)
            }
        };

        let ctype = CType::translate(&var_decl.decl_type, var_decl.scope());

        let default_value = match var_decl.default_value.as_ref() {
            Some(val_expr) => Some(Expression::translate_expression(val_expr, unit)?),
            None => None,
        };

        Ok(Variable {
            name,
            ctype,
            default_value,
            array_size: None,
        })
    }

    pub fn write_forward(&self, out: &mut fmt::Write) -> fmt::Result {
        write!(out, "extern {} {}", self.ctype, self.name)?;
        if let Some(array_size) = self.array_size.as_ref() {
            write!(out, "[{}]", array_size)?;
        }
        writeln!(out, ";")
    }

    pub fn write_impl(&self, out: &mut fmt::Write) -> fmt::Result {
        write!(out, "{} {}", self.ctype, self.name)?;
        if let Some(array_size) = self.array_size.as_ref() {
            write!(out, "[{}]", array_size)?;
        }

        if let Some(default_val) = &self.default_value {
            write!(out, " = ")?;
            default_val.write(out)?;
        }

        writeln!(out, ";")
    }

    pub fn decl_statement(&self) -> Expression {
        let mut stmt = format!("{} {}", self.ctype, self.name);
        if let Some(array_size) = self.array_size.as_ref() {
            stmt.push_str(&format!("[{}]", array_size));
        }

        if let Some(default_val) = &self.default_value {
            stmt.push_str(" = ");
            default_val.write(&mut stmt).expect("writing default val failed");
        }
        Expression::Raw(stmt)
    }

//    pub fn write_zero_uninitialized<'a>(vars: impl Iterator<Item=&'a Self>,
//                                    mut out: impl fmt::Write) -> fmt::Result {
//        for var in vars.filter(|var| var.default_value.is_none()) {
//            writeln!(out, "memset(&{}, 0, sizeof({}));", var.name, var.name)?;
//        }
//        Ok(())
//    }
}