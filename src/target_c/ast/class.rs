use std::{
    rc::Rc,
    fmt,
    collections::hash_map::HashMap,
};

use semantic::{
    self,
    Scope,
    arc_transform::{
        rc_subvalues,
        RcStrength,
    },
};
use types::{
    Type,
    ParameterizedName,
};
use target_c::ast::{
    TranslationResult,
    FunctionDecl,
    FunctionArg,
    FunctionDefinition,
    Variable,
    CType,
    Interface,
    CallingConvention,
    Block,
    Name,
    Expression,
    CastKind,
};

#[derive(Debug)]
struct ClassVTable {
    variable: Variable,
    method_impls: HashMap<String, Name>,
}

pub struct Class {
    pascal_name: ParameterizedName,
    unit_scope: Rc<Scope>,

    vtables: HashMap<usize, ClassVTable>,
    interfaces_array: Variable,

    destructor: FunctionDecl,
}

impl fmt::Debug for Class {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "Pascal class `{}` {{", self.pascal_name)?;
        if !self.vtables.is_empty() {
            writeln!(f, "\timplements: [")?;
            for (iface_id, vtable) in &self.vtables {
                writeln!(f, "\t\t{}: {}", iface_id, vtable.variable.name)?;
            }
            writeln!(f, "\t]")?;
        }
        write!(f, "}}")
    }
}

impl Class {
    pub fn translate(class: &semantic::RecordDecl,
                     type_args: &[Type],
                     unit_scope: &Rc<Scope>) -> TranslationResult<Self> {
        let name_parameterized = ParameterizedName::new_with_args(
            class.qualified_name(),
            type_args.iter().cloned(),
        );

        let (full_name, class_decl) = unit_scope.get_class_specialized(&name_parameterized).unwrap();
        if !class_decl.type_params.is_empty() {
            unimplemented!("parameterized classes (c++ backend)");
        }

        /* we also need a variable to store the list of all vtables this class implements */
        let interfaces_array = Variable {
            name: Name::class_interfaces(&name_parameterized),
            default_value: None,
            ctype: CType::Named(Name::internal_type("InterfaceImpl")),
            array_size: Some(0),
        };

        let destructor = Self::generate_destructor(&name_parameterized, class);

        Ok(Class {
//            name,
            pascal_name: full_name.clone(),
            unit_scope: unit_scope.clone(),

            /* we'll populate these later */
            vtables: HashMap::new(),
            interfaces_array,

            destructor,
        })
    }

    fn generate_destructor(pascal_name: &ParameterizedName,
                           decl: &semantic::RecordDecl)
                           -> FunctionDecl {
        let name = Name::destructor(&pascal_name);

        let instance_arg_name = Name::local("instance");
        let args = vec![
            FunctionArg {
                name: instance_arg_name.clone(),
                ctype: CType::Void.into_const().into_pointer(),
            }
        ];

        let struct_type = CType::Struct(Name::user_type(pascal_name));
        let instance_ref = Expression::cast(
            struct_type.into_const().into_pointer(),
            instance_arg_name,
            CastKind::Reinterpret
        ).deref();

        let body = decl.members.iter()
            .rev()
            .flat_map(|member| {
                let member_rc_vals = rc_subvalues(&member.decl_type, decl.scope(), None);
                let member_base = Expression::member(instance_ref.clone(), member.name.clone());

                member_rc_vals.into_iter().rev()
                    .map(move |member_val| {
                        let member_expr = Expression::translate_rc_value_expr(
                            &member_val,
                            member_base.clone()
                        );

                        let rc_strength = if member.decl_type.is_weak() {
                            RcStrength::Weak
                        } else {
                            RcStrength::Strong
                        };

                        (member_expr, rc_strength)
                    })
            })
            .map(|(member_expr, rc_strength)| {
                Expression::rc_release(member_expr, rc_strength)
            })
            .collect();

        FunctionDecl {
            name,
            args,
            return_type: CType::Void,
            calling_convention: CallingConvention::Cdecl,
            definition: FunctionDefinition::Defined(Block::new(body)),
        }
    }

    pub fn update_vtables(&mut self, interfaces: &[Interface]) {
        let this_type = Type::class_ref(self.pascal_name.clone());

        /* create a vtable variable for each interface this class implements  */
        for (iface_name, method) in self.unit_scope.get_interface_impls(&this_type) {
            let iface = interfaces.iter()
                .find(|iface| iface.pascal_name == *iface_name)
                .unwrap_or_else(|| {
                    panic!("missing interface ID for name: {}", iface_name)
                });

            let pascal_name = self.pascal_name.clone();
            let vtable = self.vtables.entry(iface.id).or_insert_with(|| {
                let vtable_name = Name::class_vtable(&pascal_name, &iface.pascal_name);

                let variable = Variable {
                    name: vtable_name,
                    array_size: None,
                    default_value: None,
                    ctype: CType::Struct(iface.vtable.name.clone().unwrap()),
                };

                ClassVTable {
                    variable,
                    method_impls: HashMap::new(),
                }
            });

            vtable.method_impls.insert(
                method.name.clone(),
                FunctionDecl::virtual_call_name(method),
            );
        }

        self.interfaces_array.array_size = Some(self.vtables.len());
    }

    pub fn is_internal(&self) -> bool {
        self.pascal_name == ParameterizedName::new_simple("System.String")
    }

    pub fn write_decl(&self, out: &mut fmt::Write) -> fmt::Result {
        self.interfaces_array.write_impl(out)?;

        for vtable_var in self.vtables.values() {
            vtable_var.variable.write_impl(out)?;
        }

        self.destructor.write_forward(out)?;

        Ok(())
    }

    pub fn write_impl(&self, out: &mut fmt::Write) -> fmt::Result {
        self.destructor.write_impl(out)?;
        Ok(())
    }

    pub fn write_init(&self, out: &mut fmt::Write) -> fmt::Result {
        for (i, (iface_id, iface_vtable)) in self.vtables.iter().enumerate() {
            for (method_name, impl_func) in &iface_vtable.method_impls {
                writeln!(
                    out, "{}.{} = &{};",
                    iface_vtable.variable.name,
                    Name::member(method_name.clone()),
                    impl_func
                )?;
            }

            writeln!(
                out, "{}[{}] = {} {{ {}, &{} }};",
                self.interfaces_array.name,
                i,
                Name::internal_symbol("InterfaceImpl"),
                iface_id,
                iface_vtable.variable.name
            )?;
        }

        writeln!(
            out, "System_Internal_InitClass(\"{}\", {}, {}, {});",
            self.pascal_name,
            self.interfaces_array.name,
            self.vtables.len(),
            self.destructor.name,
        )
    }
}