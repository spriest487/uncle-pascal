use std::{
    rc::Rc,
    fmt,
    collections::hash_map::HashMap,
};

use semantic::{
    self,
    Scope,
};
use types::{
    Type,
    ParameterizedName,
};
use target_c::ast::{
    TranslationResult,
    FunctionDecl,
    Variable,
    CType,
    Interface,
    Name,
};

struct ClassVTable {
    variable: Variable,
    method_impls: HashMap<String, Name>,
}

pub struct Class {
//    name: Name,
    pascal_name: ParameterizedName,
    unit_scope: Rc<Scope>,

    vtables: HashMap<usize, ClassVTable>,
    interfaces_array: Variable,
}

impl Class {
    pub fn translate(class: &semantic::RecordDecl,
                     type_args: &[Type],
                     unit_scope: &Rc<Scope>) -> TranslationResult<Self> {
        let name_parameterized = ParameterizedName::new_with_args(
            class.qualified_name(),
            type_args.iter().cloned()
        );

        let (full_name, class_decl) = unit_scope.get_class_specialized(&name_parameterized).unwrap();
        if !class_decl.type_params.is_empty() {
            unimplemented!("parameterized classes (c++ backend)");
        }

//        let name = Name::user_type(&full_name);

        /* we also need a variable to store the list of all vtables this class implements */
        let interfaces_array = Variable {
            name: Name::class_interfaces(&name_parameterized),
            default_value: None,
            ctype: CType::Named(Name::internal_type("InterfaceImpl")),
            array_size: Some(0),
        };

        Ok(Class {
//            name,
            pascal_name: full_name.clone(),
            unit_scope: unit_scope.clone(),

            /* we'll populate these later */
            vtables: HashMap::new(),
            interfaces_array,
        })
    }

    pub fn update_vtables(&mut self, interfaces: &[Interface]) {
        let this_type = Type::Class(self.pascal_name.clone());

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

        Ok(())
    }

    pub fn write_init(&self, out: &mut fmt::Write) -> fmt::Result {
        for (i, (iface_id, iface_vtable)) in self.vtables.iter().enumerate() {
            for (method_name, impl_func) in &iface_vtable.method_impls {
                writeln!(out, "{}.{} = &{};",
                         iface_vtable.variable.name,
                         Name::member(method_name.clone()),
                         impl_func
                )?;
            }

            writeln!(out, "{}[{}] = {} {{ {}, &{} }};",
                     self.interfaces_array.name,
                     i,
                     Name::internal_symbol("InterfaceImpl"),
                     iface_id,
                     iface_vtable.variable.name
            )?;
        }

        writeln!(out, "System_Internal_InitClass(\"{}\", {}, {});",
                 self.pascal_name,
                 self.interfaces_array.name,
                 self.vtables.len())
    }
}