use std::fmt::{
    self,
};
use semantic;
use node::{
    Identifier,
    FunctionArgModifier,
    FunctionModifier,
    FunctionLocalDecl,
};
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

        let name = Self::translate_name(pascal_decl);

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
    pub fn interface_call_name(interface: &Identifier,
                               func_name: &str,
                               for_type: &Identifier) -> String {
        let qualified_impl = interface.child(func_name);

        format!("{}_{}", identifier_to_c(&qualified_impl), identifier_to_c(&for_type))
    }

    pub fn translate_name(decl: &semantic::FunctionDecl) -> String {
        match decl.implements.as_ref() {
            Some(implements) => {
                let for_type_name = decl.scope().full_type_name(&implements.for_type).unwrap();

                Self::interface_call_name(
                    &implements.interface,
                    &decl.name,
                    &for_type_name,
                )
            }

            None => {
                let qualified = decl.scope().namespace_qualify(&decl.name);
                identifier_to_c(&qualified)
            }
        }
    }

    pub fn translate(function: &semantic::Function,
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

        if !decl.return_type.is_void() {
            let result_decl = Variable {
                default_value: None,
                name: "result".to_string(),
                ctype: decl.return_type.clone(),
            };

            body.statements.insert(0, result_decl.decl_statement());

            /* it shouldn't be necessary to zero-initialize the result, because the typechecker
            should check that result is always initialized by the program */
            // body.statements.insert(1, Expression::from("memset(&result, 0, sizeof(result)); "));
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

        for arg in rc_args.iter() {
            body.statements.insert(0, Expression::Raw(
                format!("System_Internal_Rc_Retain({});", arg.name)
            ));
            body.statements.push(Expression::Raw(
                format!("System_Internal_Rc_Release({})", arg.name)
            ));
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