use std::fmt::{
    self,
};
use semantic;
use node::{
    FunctionArgModifier,
    FunctionModifier,
    FunctionKind,
    FunctionLocalDecl,
};
use types::Type;
use target_c::{
    ast::{
        CType,
        TranslationResult,
        Block,
        TranslationUnit,
        Expression,
        Variable,
    },
    identifier_to_c,
};

pub struct FunctionArg {
    pub name: String,
    pub ctype: CType,
}

impl fmt::Display for FunctionArg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", self.ctype, self.name)
    }
}

pub enum FunctionDefinition {
    None,
    External(String),
    Defined(Block),
}

pub enum CallingConvention {
    Cdecl,
    Stdcall,
}

impl fmt::Display for CallingConvention {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CallingConvention::Cdecl => write!(f, "__cdecl"),
            CallingConvention::Stdcall => write!(f, "__stdcall"),
        }
    }
}

pub struct FunctionDecl {
    pub name: String,
    pub return_type: CType,
    pub calling_convention: CallingConvention,
    pub args: Vec<FunctionArg>,
    pub definition: FunctionDefinition,
}

impl<'a> From<&'a semantic::FunctionDecl> for FunctionDecl {
    fn from(pascal_decl: &semantic::FunctionDecl) -> Self {
        let return_type = pascal_decl.return_type.as_ref()
            .map(|return_type| CType::translate(return_type, pascal_decl.scope()))
            .unwrap_or_else(|| CType::Void);

        let qualified_name = pascal_decl.scope().namespace_qualify(&pascal_decl.name);
        let name = identifier_to_c(&qualified_name);

        let calling_convention = if pascal_decl.modifiers.contains(&FunctionModifier::Stdcall) {
            // todo: semantic error earlier?
            assert!(!pascal_decl.modifiers.contains(&FunctionModifier::Cdecl),
                    "function decl `{}` has multiple calling conventions",
                    pascal_decl.name);
            CallingConvention::Stdcall
        } else {
            CallingConvention::Cdecl
        };

        let args = pascal_decl.args.iter()
            .map(|arg_decl| {
                let ctype_base = CType::translate(&arg_decl.decl_type, &arg_decl.scope());
                let ctype = match &arg_decl.modifier {
                    | None => ctype_base,
                    | Some(FunctionArgModifier::Const)
                    => ctype_base.into_const(),

                    | Some(FunctionArgModifier::Var)
                    | Some(FunctionArgModifier::Out)
                    => ctype_base.into_ref(),
                };

                FunctionArg {
                    name: arg_decl.name.clone(),
                    ctype,
                }
            })
            .collect();

        /* for function definitions, this field gets replaced later */
        let extern_modifier = pascal_decl.modifiers.iter()
            .filter_map(|func_mod| match func_mod {
                FunctionModifier::External(extern_name) => Some(extern_name),
                _ => None,
            })
            .next();

        let definition = match extern_modifier {
            Some(extern_name) => {
                if extern_name.shared_lib.is_some() {
                    unimplemented!("DLL imports");
                }

                /* if no `name` part is present in the extern decl, the unqualified
                name of the function is used */
                FunctionDefinition::External(extern_name.symbol_name.as_ref()
                    .unwrap_or_else(|| &pascal_decl.name)
                    .to_string())
            }
            None => FunctionDefinition::None,
        };

        FunctionDecl {
            name,
            calling_convention,
            args,
            return_type,
            definition,
        }
    }
}

impl FunctionDecl {
    pub fn from_function(function: &semantic::Function,
                         unit: &mut TranslationUnit)
                         -> TranslationResult<Self> {
        let local_vars: Vec<_> = function.local_decls.iter()
            .filter_map(|decl| match decl {
                FunctionLocalDecl::Var(var) => Some(var),
                _ => None,
            })
            .cloned()
            .collect();

        let decl = FunctionDecl::from(&function.decl);

        let mut body = Block::translate(&function.block, Some(&local_vars), unit)?;

        let result_default = match function.decl.kind {
            /*
                in a constructor, the result is initialized with an allocated and
                zero-initialized instance of the class
            */
            FunctionKind::Constructor => {
                // need the name of the class, not the actual c type (which is a pointer)
                let class_name = match function.decl.return_type.as_ref() {
                    Some(Type::Class(name)) => {
                        name
                    }
                    _ => panic!("constructor must return a class type"),
                };

                let class_c_name = identifier_to_c(class_name);

                let rc_alloc = Expression::function_call("System_Internal_Rc_GetMem", vec![
                    Expression::function_call("sizeof", vec![Expression::raw(&class_c_name)]),
                    Expression::string_literal(&class_name.to_string())
                ]);

                Some(Expression::static_cast(
                    CType::Struct(class_c_name).into_pointer(),
                    rc_alloc,
                ))
            }

            _ => None,
        };

        if !decl.return_type.is_void() {
            let result_decl = Variable {
                default_value: result_default,
                name: "result".to_string(),
                ctype: decl.return_type.clone(),
            };

            body.statements.insert(0, result_decl.decl_statement());

            if function.decl.kind != FunctionKind::Constructor {
                body.statements.insert(1, Expression::from("memset(&result, 0, sizeof(result)); "));
            }
        }

        /*
            retain rc args to non-destructor functions
            this is kind of a hack to make sure temporary values used as args get rc'd
            destructors mustn't change the ref count of their only arg, the dead object,
            or bad things happen
            it's safe not to do this in a constructor, the object under construction exists only
            in the result position
        */
        let rc_args: Vec<_> = function.decl.args.iter()
            .filter(|arg| {
                arg.modifier != Some(FunctionArgModifier::Out)
                    && arg.decl_type.is_class()
            })
            .collect();

        if function.decl.kind == FunctionKind::Function {
            for arg in rc_args.iter() {
                body.statements.insert(0, Expression::Raw(
                    format!("System_Internal_Rc_Retain({});", arg.name)
                ));
                body.statements.push(Expression::Raw(
                    format!("System_Internal_Rc_Release({})", arg.name)
                ));
            }
        }

        if !decl.return_type.is_void() {
            body.statements.push(Expression::from("return result"));
        }

        Ok(FunctionDecl {
            definition: FunctionDefinition::Defined(body),
            ..decl
        })
    }

    pub fn args_list(&self) -> String {
        self.args.iter()
            .map(|arg| arg.to_string())
            .collect::<Vec<_>>()
            .join(", ")
    }

    pub fn write_forward(&self, mut out: impl fmt::Write) -> fmt::Result {
        writeln!(
            out,
            "static {} {} {}({});",
            self.return_type,
            self.calling_convention,
            self.name,
            self.args_list()
        )
    }

    pub fn write_impl(&self, mut out: impl fmt::Write) -> fmt::Result {
        match &self.definition {
            FunctionDefinition::None => {}

            FunctionDefinition::External(extern_name) => {
                writeln!(out, "namespace ffi {{")?;
                writeln!(out, "extern \"C\" {} {} {}({});",
                         self.return_type,
                         self.calling_convention,
                         extern_name,
                         self.args_list()
                )?;
                writeln!(out, "}}")?;

                writeln!(out, "{} {}({}) {{",
                         self.return_type,
                         self.name,
                         self.args_list())?;

                if !self.return_type.is_void() {
                    write!(out, "return ")?;
                }
                writeln!(out, "ffi::{}({});",
                         extern_name,
                         self.args.iter()
                             .map(|arg| arg.name.clone())
                             .collect::<Vec<_>>()
                             .join(", "))?;
                writeln!(out, "}}")?;
            }

            FunctionDefinition::Defined(body) => {
                writeln!(out, "{} {}({}) ",
                         self.return_type,
                         self.name,
                         self.args_list())?;

                body.write(&mut out)?;
            }
        }

        Ok(())
    }
}