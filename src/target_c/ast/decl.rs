use std::fmt;
use target_c::ast::{
    rc_getmem,
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
    Identifier,
    UnitDecl,
    TypeDecl,
    RecordKind,
};
use types::{
    Type,
    ParameterizedName,
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
    Field(Box<Variable>),
    AnonymousUnion(Vec<StructMember>),
    Struct(Box<Struct>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Struct {
    pub name: Option<Name>,
    pub extends: Option<Name>,
    pub members: Vec<StructMember>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructDecl {
    pub decl: Struct,
    pub ctor: FunctionDecl,
}

pub enum Declaration {
    Variable(Variable),
    Function(FunctionDecl),
    UsingAlias(Name, CType),

    /* when we encounter a struct declaration, we store it in the unit so we can accumulate
    all specializations globally, so we only need to store the pascal ID here to look it up */
    Struct(Identifier),
}

impl Declaration {
    pub fn translate_decl(decl: &semantic::UnitDecl,
                          unit: &mut TranslationUnit)
                          -> TranslationResult<Vec<Declaration>> {
        match decl {
            UnitDecl::Function(func_decl) => {
                let function = FunctionDecl::translate_decl(func_decl, unit)?;
                Ok(vec!(Declaration::Function(function)))
            }

            UnitDecl::Type(ref type_decl) =>
                match type_decl {
                    | TypeDecl::Record(record_decl) => {
                        unit.declare_struct(record_decl.clone());

                        Ok(vec![
                            Declaration::Struct(record_decl.qualified_name()),
                        ])
                    }

                    | TypeDecl::Enumeration(enum_decl) => {
                        let name = Name::user_type(&ParameterizedName::new_simple(
                            enum_decl.qualified_name()));

                        Ok(vec!(Declaration::UsingAlias(
                            name,
                            CType::Named(Name::internal_type("UInt64")),
                        )))
                    }

                    | TypeDecl::Set(set_decl) => {
                        let name = Name::user_type(&ParameterizedName::new_simple(
                            set_decl.qualified_name()));

                        let alias = Declaration::UsingAlias(
                            name,
                            CType::Named(Name::internal_type("Set<System_UInt64>".to_string()),
                            ),
                        );

                        Ok(vec!(alias))
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

                if func_impl.decl.implements.is_some() {
                    let vcall_adaptor = FunctionDecl::virtual_call_adaptor(&func_impl.decl, unit)?;

                    Ok(vec![
                        Declaration::Function(definition),
                        Declaration::Function(vcall_adaptor)
                    ])
                } else {
                    Ok(vec![Declaration::Function(definition)])
                }
            }

            node::Implementation::Decl(decl) => {
                Self::translate_decl(decl, unit)
            }
        }
    }

    pub fn write_forward(&self, unit: &TranslationUnit, out: &mut fmt::Write) -> fmt::Result {
        match self {
            Declaration::Variable(variable) => {
                variable.write_forward(out)
            }

            Declaration::Function(function) =>
                function.write_forward(out),

            Declaration::Struct(record) => {
                for instantiation in unit.struct_instantiations(record) {
                    instantiation.struct_decl.decl.write_forward(out)?;
                    instantiation.struct_decl.ctor.write_forward(out)?;
                }
                Ok(())
            }

            Declaration::UsingAlias(alias, target) =>
                writeln!(out, "using {} = {};", alias, target)
        }
    }

    pub fn write_impl(&self, unit: &TranslationUnit, out: &mut fmt::Write) -> fmt::Result {
        match self {
            Declaration::Variable(variable) => {
                variable.write_impl(out)
            }

            Declaration::Function(function) =>
                function.write_impl(out),

            Declaration::Struct(record) => {
                for instantiation in unit.struct_instantiations(record) {
                    instantiation.struct_decl.decl.write_def(out)?;
                    instantiation.struct_decl.ctor.write_impl(out)?;
                }
                Ok(())
            }

            Declaration::UsingAlias(_alias, _target) =>
                Ok(())
        }
    }
}

impl Struct {
    pub fn translate(record_decl: &semantic::RecordDecl,
                     type_args: &[Type],
                     unit: &mut TranslationUnit)
                     -> TranslationResult<StructDecl> {
        assert!(!record_decl.members.is_empty() || record_decl.variant_part.is_some(),
                "structs must have at least one member");

        assert_eq!(0, record_decl.type_params.len(),
                   "Struct::translate expects fully specialized record decls");

        let parameterized_name = ParameterizedName::new_with_args(
            record_decl.scope().namespace_qualify(&record_decl.name),
            type_args.iter().cloned(),
        );
        let name = Name::user_type(&parameterized_name);

        let mut members: Vec<_> = record_decl.members.iter()
            .map(|member_decl| {
                let ctype = CType::translate(&member_decl.decl_type, record_decl.scope(), unit)?;

                Ok(StructMember::Field(Box::new(Variable {
                    name: Name::member(member_decl.name.clone()),
                    default_value: None,
                    array_size: None,
                    ctype,
                })))
            })
            .collect::<TranslationResult<_>>()?;

        if let Some(variant_part) = &record_decl.variant_part {
            let tag = Variable {
                name: Name::member(variant_part.tag.name.clone()),
                ctype: CType::translate(&variant_part.tag.decl_type, record_decl.scope(), unit)?,
                default_value: None,
                array_size: None,
            };
            members.push(StructMember::Field(Box::new(tag)));

            let cases = variant_part.cases.iter()
                .map(|case| {
                    /* each case is an anonymous struct inside the union, containing all the
                    membes of that variant */
                    let case_struct_members: Vec<_> = case.members.iter()
                        .map(|member| {
                            let ctype = CType::translate(&member.decl_type, record_decl.scope(), unit)?;

                            Ok(StructMember::Field(Box::new(Variable {
                                name: Name::member(member.name.clone()),
                                ctype,
                                default_value: None,
                                array_size: None,
                            })))
                        })
                        .collect::<TranslationResult<_>>()?;

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

        let constructor_decl = Struct::constructor_decl(&parameterized_name, &record_decl, unit)?;

        let struct_decl = Struct {
            name: Some(name),
            members,
            extends,
        };

        Ok(StructDecl {
            decl: struct_decl,
            ctor: constructor_decl,
        })
    }

    pub fn write_forward(&self, out: &mut fmt::Write) -> fmt::Result {
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

        for member in &self.members {
            member.write(out)?;
        }

        writeln!(out, "}};")
    }

    // todo: this should probably be based on semantic::RecordDecl instead
    fn constructor_decl(struct_name: &ParameterizedName,
                        record: &semantic::RecordDecl,
                        unit: &mut TranslationUnit)
                        -> TranslationResult<FunctionDecl> {
        let name = Name::constructor(&struct_name);
        let result_name = Name::local("result");

        let all_fields: Vec<_> = record.all_members().collect();

        /* the args of a constructor are Options of each individual member */
        let args: Vec<_> = all_fields.iter().enumerate()
            .map(|(mem_num, member)| {
                let member_arg_type = {
                    let member_type = CType::translate(&member.decl_type, record.scope(), unit)?;
                    CType::Named(Name::internal_type(format!("Option<{}>", member_type)))
                };

                Ok(FunctionArg {
                    name: Name::local(format!("arg{}", mem_num)),
                    ctype: member_arg_type,
                })
            })
            .collect::<TranslationResult<_>>()?;

        let struct_cname = Name::user_type(struct_name);

        let struct_type = CType::Struct(struct_cname.clone());
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
        statements.push(Expression::local_decl(return_type.clone(), Name::local("result"), None));
        match record.kind {
            /* rc-allocate new class instance */
            RecordKind::Class => {
                statements.push(Expression::binary_op(
                    result_name.clone(), "=",
                    Expression::cast(
                        return_type.clone(),
                        rc_getmem(struct_cname.clone(), &record.qualified_name()),
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
            let result_member = Expression::member(result_val.clone(), member.name.clone());
            let arg_name = Name::local(format!("arg{}", mem_num));

            let deref_arg = Expression::unary_op("*", arg_name.clone(), true);
            let assign_member = Expression::binary_op(result_member, "=", deref_arg);

            let assign_if_present = Expression::if_then(arg_name, assign_member);

            statements.push(assign_if_present);
        }

        statements.push(Expression::return_value(result_name));

        let definition = FunctionDefinition::Defined(Block::new(statements));

        Ok(FunctionDecl {
            name,
            args,
            return_type,
            calling_convention: CallingConvention::Cdecl,
            definition,
        })
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
        let name = if !local {
            let full_id = var_decl.scope().namespace_qualify(&var_decl.name);
            Name::user_symbol(&full_id)
        } else {
            Name::local(var_decl.name.clone())
        };

        let ctype = CType::translate(&var_decl.decl_type, var_decl.scope(), unit)?;

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