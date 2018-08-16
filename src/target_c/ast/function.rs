use std::fmt::{
    self,
    Write,
};
use semantic;
use node::{
    FunctionArgModifier,
    FunctionModifier,
    FunctionKind,
    FunctionLocalDecl,
};
use target_c::writer::{
    identifier_to_c,
    write_block,
    write_vars,
    write_consts,
    default_initialize_vars,
    release_vars,
};
use types::Type;
use target_c::{
    ast::CType,
    writer::{
        module_globals::ModuleGlobals
    },
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
    Defined(String),
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
            .map(|return_type| CType::from_pascal(return_type, pascal_decl.scope()))
            .unwrap_or_else(|| CType::Void);

        let qualified_name = pascal_decl.scope().qualify_local_name(&pascal_decl.name);
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
                let ctype_base = CType::from_pascal(&arg_decl.decl_type, &arg_decl.scope());
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
    pub fn from_function(function: &semantic::Function, globals: &mut ModuleGlobals) -> Result<FunctionDecl, fmt::Error> {
        let mut body = String::new();
        writeln!(&mut body, "{{")?;

        let mut all_local_vars = Vec::new();
        for local_decl in function.local_decls.iter() {
            match local_decl {
                FunctionLocalDecl::Consts(local_consts) => {
                    write_consts(&mut body, local_consts, globals)?;
                }

                FunctionLocalDecl::Vars(local_vars) => {
                    write_vars(&mut body, local_vars)?;
                    default_initialize_vars(&mut body, local_vars.decls.iter())?;
                    all_local_vars.extend(local_vars.decls.iter());
                }

                FunctionLocalDecl::NestedFunction(_) =>
                    unimplemented!("nested functions"),
            }
        }

        if function.decl.kind == FunctionKind::Constructor {
            //the actual return type is an Rc, but we need to pass the class type to sizeof
            let constructed_class_c_name = match function.decl.return_type.as_ref().unwrap() {
                Type::Class(name) => {
                    identifier_to_c(&name)
                }
                _ => panic!("constructor must return a class type"),
            };

            writeln!(&mut body, "result = ({}*)System_Internal_Rc_GetMem(sizeof(struct {}), \"{}\");",
                     constructed_class_c_name,
                     constructed_class_c_name,
                     function.decl.return_type.as_ref().expect("constructor must have return type"))?;
        }

        let rc_args: Vec<_> = function.decl.args.iter()
            .filter(|arg| {
                arg.modifier != Some(FunctionArgModifier::Out)
                    && arg.decl_type.is_class()
            })
            .collect();

        // retain rc args to non-destructor functions
        // this is kind of a hack to make sure temporary values used as args get rc'd
        // destructors musn't change the ref count of their only arg, the dead object, or bad things
        // happen
        // it's safe to do this with a constructor because the object under construction exists only
        // in the result position
        if function.decl.kind == FunctionKind::Function {
            for arg in rc_args.iter() {
                writeln!(&mut body, "System_Internal_Rc_Retain({});", arg.name)?;
            }
        }

        write_block(&mut body, &function.block, globals)?;
        release_vars(&mut body, all_local_vars.into_iter()
            .filter(|var| var.name != "result"))?;

        if function.decl.kind == FunctionKind::Function {
            //release args
            for arg in rc_args.iter() {
                writeln!(&mut body, "System_Internal_Rc_Release({});", arg.name)?;
            }
        }

        let return_type_c = function.decl.return_type.as_ref()
            .map(|return_type| CType::from_pascal(return_type, function.scope()));

        match return_type_c {
            Some(_) => writeln!(&mut body, "return result;")?,
            None => (),
        }

        writeln!(&mut body, "}}")?;
        writeln!(&mut body)?;

        Ok(FunctionDecl {
            definition: FunctionDefinition::Defined(body),
            ..FunctionDecl::from(&function.decl)
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
                         self.calling_convention,
                         self.return_type,
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
                writeln!(out, "{} {}({}) {{",
                         self.return_type,
                         self.name,
                         self.args_list())?;
                out.write_str(body)?;
                writeln!(out, "}}")?;
            }
        }

        Ok(())
    }
}