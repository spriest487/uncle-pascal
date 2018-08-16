use std::{
    rc::Rc,
    fmt,
    collections::hash_map::{
        HashMap,
    },
};

use semantic::{
    self,
    Scope,
};
use node::Identifier;
use types::Type;
use target_c::{
    ast::{
        TranslationResult,
        FunctionDecl,
        Variable,
        CType,
        Interface,
    },
    identifier_to_c,
};

struct ClassVTable {
    variable: Variable,
    method_impls: HashMap<String, String>,
}

pub struct Class {
    name: String,
    pascal_name: String,
    unit_scope: Rc<Scope>,

    destructor: Option<String>,

    vtables: HashMap<usize, ClassVTable>,
    interfaces_array: Variable,
}

impl Class {
    pub fn translate(class: &semantic::RecordDecl,
                     unit_scope: Rc<Scope>) -> TranslationResult<Self> {
        let (full_name, _) = unit_scope.get_class(&Identifier::from(&class.name)).unwrap();
        let destructor = unit_scope.get_destructor(&Type::Class(full_name.clone()))
            .map(|(_, dtor_func)| FunctionDecl::translate_name(dtor_func));

        let name = identifier_to_c(&full_name);

        /* we also need a variable to store the list of all vtables this class implements */
        let interfaces_array = Variable {
            name: format!("{}_Interfaces", identifier_to_c(&full_name)),
            default_value: None,
            ctype: CType::Named("System_Internal_InterfaceImpl".to_string()),
            array_size: Some(0),
        };

        Ok(Class {
            name,
            pascal_name: full_name.to_string(),
            unit_scope: unit_scope.clone(),

            destructor,

            /* we'll populate these later */
            vtables: HashMap::new(),
            interfaces_array,
        })
    }

    pub fn update_vtables(&mut self, interfaces: &[Interface]) {
        let this_type = Type::Class(Identifier::from(&self.pascal_name));

        /* create a vtable variable for each interface this class implements  */
        for (iface_name, method) in self.unit_scope.get_interface_impls(&this_type) {
            let iface = interfaces.iter()
                .find(|iface| iface.pascal_name == *iface_name)
                .unwrap_or_else(|| {
                    panic!("missing interface ID for name: {}", iface_name)
                });

            let name = self.name.clone();
            let vtable = self.vtables.entry(iface.id).or_insert_with(|| {
                let vtable_name = format!("{}_VTable_{}", name, iface.id);

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
        self.pascal_name == "System.String"
    }

    pub fn write_decl(&self, out: &mut fmt::Write) -> fmt::Result {
        self.interfaces_array.write_impl(out)?;

        for (_, vtable_var) in self.vtables.iter() {
            vtable_var.variable.write_impl(out)?;
        }

        Ok(())
    }

    pub fn write_init(&self, out: &mut fmt::Write) -> fmt::Result {
        for (i, (iface_id, iface_vtable)) in self.vtables.iter().enumerate() {
            for (method_name, impl_func) in iface_vtable.method_impls.iter() {
                writeln!(out, "{}.{} = &{};", iface_vtable.variable.name, method_name, impl_func)?;
            }

            writeln!(out, "{}[{}] = {} {{ {}, &{} }};",
                     self.interfaces_array.name,
                     i,
                     "System_Internal_InterfaceImpl",
                     iface_id,
                     iface_vtable.variable.name)?;
        }

        let destructor_ptr = match &self.destructor {
            Some(destructor) => {
                format!("(System_Internal_Destructor)&{}", destructor)
            }
            None =>
                "nullptr".to_string(),
        };

        writeln!(out, "System_Internal_InitClass(\"{}\", {}, {}, {});",
                 self.pascal_name,
                 destructor_ptr,
                 self.interfaces_array.name,
                 self.vtables.len())
    }
}