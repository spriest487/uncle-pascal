use std::collections::HashMap;
use node::{
    Identifier,
    FunctionArgModifier,
};
use target_c::{
    identifier_to_c,
    ast::{
        TranslationResult,
        Struct,
        StructMember,
        Variable,
        CType,
        FunctionDecl,
        FunctionArg,
        FunctionDefinition,
        Expression,
        Block,
        CallingConvention,
    },
};
use semantic::{
    self,
    Scope,
};
use types::Type;

pub struct Interface {
    pub id: usize,
    pub pascal_name: Identifier,

    pub vtable: Struct,
    pub methods: HashMap<String, FunctionDecl>,
}

impl Interface {
    pub fn translate(decl: &semantic::InterfaceDecl, id: usize)
                     -> TranslationResult<Self> {
        let vtable_members = decl.methods.iter()
            .map(|(method_name, method)| {
                let func_type = CType::translate(
                    &Type::Function(Box::new(method.clone())),
                    decl.scope(),
                );

                StructMember::Field(Variable {
                    name: method_name.clone(),
                    ctype: func_type,
                    default_value: None,
                    array_size: None,
                })
            })
            .collect();

        let pascal_name = decl.qualified_name();

        let vtable_name = format!("{}_VTable", identifier_to_c(&pascal_name));

        let methods = decl.methods.iter()
            .map(|(method_name, method)| {
                let method_func = Self::virtual_method_call(
                    id,
                    &pascal_name,
                    &vtable_name,
                    method_name,
                    method,
                    decl.scope()
                );

                (method_name.clone(), method_func)
            })
            .collect();

        let vtable = Struct {
            name: Some(vtable_name),
            members: vtable_members,
            extends: None,
        };

        Ok(Interface {
            id,
            vtable,
            pascal_name,
            methods,
        })
    }

    fn virtual_method_call(iface_id: usize,
                           iface_pascal_name: &Identifier,
                           vtable_name: &str,
                           method_name: &str,
                           method: &semantic::FunctionSignature,
                           scope: &Scope) -> FunctionDecl {
        let name = identifier_to_c(&iface_pascal_name
            .child(&method_name)
            .child("VirtualCall"));

        let calling_convention = CallingConvention::from_modifiers(&method.modifiers);

        let return_type = method.return_type.as_ref()
            .map(|return_ty| CType::translate(return_ty, scope))
            .unwrap_or(CType::Void);

        let args = method.args.iter().enumerate()
            .map(|(arg_num, method_arg)| {
                let base_type = CType::translate(&method_arg.decl_type, scope);
                let ctype = match method_arg.modifier {
                    | Some(FunctionArgModifier::Var)
                    | Some(FunctionArgModifier::Out)
                    => base_type.into_ref(),

                    | Some(FunctionArgModifier::Const)
                    | None
                    => base_type,
                };

                FunctionArg {
                    name: format!("arg{}", arg_num),
                    ctype,
                }
            })
            .collect();

        let find_vtable = Expression::function_call("System_Internal_FindVTable", vec![
            Expression::raw("arg0"),
            Expression::raw(iface_id.to_string()),
        ]);

        let vtable_ptr_type = CType::Struct(vtable_name.to_string()).into_const().into_pointer();
        let ptr_to_vtable = Expression::static_cast(vtable_ptr_type.clone(), find_vtable);
        let vtable_var = Expression::local_decl(vtable_ptr_type, "vtable", ptr_to_vtable);

        let all_args: Vec<_> = (0..method.args.len())
            .map(|arg_num| Expression::raw(format!("arg{}", arg_num)))
            .collect();

        let invoke_vtable_impl = Expression::function_call(
            Expression::member(Expression::unary_op("*", "vtable", true), method_name.to_string()),
            all_args,
        );

        let definition = FunctionDefinition::Defined(Block {
            statements: vec![
                vtable_var,
                Expression::return_value(invoke_vtable_impl),
            ]
        });

        let decl = FunctionDecl {
            name,
            calling_convention,
            return_type,
            args,
            definition,
        };

        decl
    }
}