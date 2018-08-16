use std::{
    fmt,
};
use target_c::{
    identifier_to_c,
    ast::{
        CType,
        FunctionDecl,
        TranslationResult,
        TranslationUnit,
        Expression,
        CallingConvention,
        FunctionDefinition,
        FunctionArg,
        Block,
    },
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
    pub name: String,
    pub ctype: CType,
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
    name: Option<String>,
    extends: Option<String>,
    members: Vec<StructMember>,
}

pub enum Declaration {
    Variable(Variable),
    Function(FunctionDecl),
    Struct(Struct),
    UsingAlias(String, String),
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
                        let name = identifier_to_c(&enum_decl.qualified_name());
                        Ok(vec!(Declaration::UsingAlias(
                            name,
                            "System_UInt64".to_string(),
                        )))
                    }

                    | TypeDecl::Set(set_decl) => {
                        //todo
                        let name = identifier_to_c(&set_decl.qualified_name());
                        Ok(vec!(Declaration::UsingAlias(
                            name,
                            "std::unordered_set<System_UInt64>".to_string(),
                        )))
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
                Ok(vec!(Declaration::Function(definition)))
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
                record.write_impl(&mut out),

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

        let name = identifier_to_c(&record_decl.scope().namespace_qualify(&record_decl.name));

        let mut members: Vec<_> = record_decl.members.iter()
            .map(|member_decl| {
                let ctype = CType::translate(&member_decl.decl_type, record_decl.scope());

                Ok(StructMember::Field(Variable {
                    name: member_decl.name.clone(),
                    default_value: None,
                    ctype,
                }))
            })
            .collect::<TranslationResult<_>>()?;

        if let Some(variant_part) = &record_decl.variant_part {
            let tag = Variable {
                name: variant_part.tag.name.clone(),
                ctype: CType::translate(&variant_part.tag.decl_type, record_decl.scope()),
                default_value: None,
            };
            members.push(StructMember::Field(tag));

            let cases = variant_part.cases.iter()
                .map(|case| {
                    /* each case is an anonymous struct inside the union, containing all the
                    membes of that variant */
                    let case_struct_members: Vec<_> = case.members.iter()
                        .map(|member| {
                            StructMember::Field(Variable {
                                name: member.name.clone(),
                                ctype: CType::translate(&member.decl_type, record_decl.scope()),
                                default_value: None,
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
            RecordKind::Class => Some("System_Internal_Object".to_string()),
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

    pub fn write_impl(&self, out: &mut fmt::Write) -> fmt::Result {
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
    fn constructor_decl(struct_name: &str, record: &semantic::RecordDecl) -> FunctionDecl {
        let name = format!("{}_Internal_Constructor", struct_name);

        let all_fields: Vec<_> = record.all_members().collect();

        /* the args of a constructor are Options of each individual member */
        let args: Vec<_> = all_fields.iter().enumerate()
            .map(|(mem_num, member)| FunctionArg {
                name: format!("arg{}", mem_num),
                ctype: {
                    let member_type = CType::translate(&member.decl_type, record.scope());
                    CType::Named(format!("System_Internal_Option<{}>", member_type))
                },
            })
            .collect();

        let struct_type = CType::Struct(struct_name.to_string());
        let (return_type, member_access) = match record.kind {
            RecordKind::Class => (struct_type.into_pointer(), "->"),
            RecordKind::Record => (struct_type, "."),
        };

        let mut statements = Vec::new();
        statements.push(Expression::raw(format!("{} result", return_type)));
        match record.kind {
            /* rc-allocate new class instance */
            RecordKind::Class => {
                statements.push(Expression::binary_op(
                    "result", "=",
                    Expression::static_cast(
                        return_type.clone(),
                        Expression::function_call("System_Internal_Rc_GetMem", vec![
                            Expression::function_call("sizeof", vec![Expression::from(struct_name)]),
                            Expression::string_literal(&record.qualified_name().to_string())
                        ]),
                    ),
                ));
            }

            /* zero-initialize a new record on the stack */
            RecordKind::Record => {
                statements.push(Expression::function_call("std::memset", vec![
                    Expression::raw("&result"),
                    Expression::raw(0),
                    Expression::function_call("sizeof", vec![Expression::from(struct_name)])
                ]));
            }
        }

        for (mem_num, member) in all_fields.iter().enumerate() {
            let result_member = format!("result{}{}", member_access, member.name);
            let arg_name = format!("arg{}", mem_num);
            statements.push(Expression::from(format!(
                "if ({}) {} = *{}",
                arg_name,
                result_member,
                arg_name
            )));
        }

        statements.push(Expression::raw("return result"));

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
                nested_struct.write_impl(out)?;
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
                identifier_to_c(&full_id)
            }
            true => {
                var_decl.name.clone()
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
        })
    }

    pub fn write_forward(&self, out: &mut fmt::Write) -> fmt::Result {
        writeln!(out, "extern {} {};", self.ctype, self.name)
    }

    pub fn write_impl(&self, out: &mut fmt::Write) -> fmt::Result {
        write!(out, "{} {}", self.ctype, self.name)?;

        if let Some(default_val) = &self.default_value {
            write!(out, " = ")?;
            default_val.write(out)?;
        }

        writeln!(out, ";")
    }

    pub fn decl_statement(&self) -> Expression {
        let mut stmt = format!("{} {}", self.ctype, self.name);
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