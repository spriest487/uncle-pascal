use std::{
    fmt,
    rc::Rc,
};
use linked_hash_set::LinkedHashSet;
use node::{
    Identifier,
    Implementation,
    UnitDecl,
    TypeDecl,
    RecordKind,
};
use target_c::ast::{
    rc_release,
    TranslationResult,
    Declaration,
    Block,
    Class,
    Interface,
    Expression,
    Name,
    Variable,
    CType,
    CastKind,
};
use semantic::{
    self,
    Scope,
};

pub struct TranslationUnit {
    decls: Vec<Declaration>,

    classes: Vec<Class>,
    interfaces: Vec<Interface>,
    string_literals: LinkedHashSet<String>,

    initialization: Vec<Block>,
    finalization: Vec<Block>,
}

impl TranslationUnit {
    pub fn decls(&self) -> &[Declaration] {
        &self.decls
    }

    pub fn initialization(&self) -> &[Block] {
        &self.initialization
    }

    pub fn finalization(&self) -> &[Block] {
        &self.finalization
    }

//    pub fn interfaces(&self) -> &[Interface] {
//        &self.interfaces
//    }

    fn add_implementation_decl(&mut self,
                               decl: &semantic::Implementation,
                               unit_scope: Rc<Scope>)
                               -> TranslationResult<()> {
        match decl {
            Implementation::Decl(decl) => {
                self.add_decl(decl, unit_scope)?;
            }
            _ => {
                let impl_decl = Declaration::translate_impl(decl, self)?;
                self.decls.extend(impl_decl);
            }
        }

        Ok(())
    }

    fn string_literal_index_to_name(index: usize) -> Name {
        Name::internal_symbol(format!("StringLiteral_{}", index))
    }

    pub fn string_literal_name(&mut self, string_literal: &str) -> Name {
        self.string_literals.insert_if_absent(string_literal.to_string());

        let index = self.string_literals.iter()
            .enumerate()
            .filter_map(|(index, existing)| if existing == string_literal {
                Some(index)
            } else {
                None
            })
            .next()
            .unwrap();

        Self::string_literal_index_to_name(index)
    }

    pub fn declare_string_literals(&self, out: &mut impl fmt::Write) -> fmt::Result {
        for (index, _) in self.string_literals.iter().enumerate() {
            let name = Self::string_literal_index_to_name(index);

            let var = Variable {
                name,
                ctype: CType::Struct(Name::user_type(&Identifier::from("System.String")))
                    .into_pointer(),
                default_value: None,
                array_size: None,
            };

            var.write_impl(out)?;
        }

        Ok(())
    }

    pub fn init_string_literals(&self, mut out: impl fmt::Write) -> fmt::Result {
        for (index, literal) in self.string_literals.iter().enumerate() {
            let name = Self::string_literal_index_to_name(index);

            let byte_type = CType::Named(Name::user_type(&Identifier::from("System.Byte")));

            /* StringFromBytes takes ^Byte currently, so we have to do two casts (one into
             `const Byte*`, the next into `Byte*` */
            let literal_as_bytes = Expression::cast(
                byte_type.clone().into_const().into_pointer(),
                Expression::string_literal(literal),
                CastKind::Reinterpret,
            );

            let literal_as_mut_bytes = Expression::cast(
                byte_type.into_pointer(),
                literal_as_bytes,
                CastKind::Const,
            );

            let init_expr = Expression::binary_op(
                name,
                "=",
                Expression::function_call(
                    Name::user_symbol(&Identifier::from("System.StringFromBytes")),
                    vec![
                        literal_as_mut_bytes,
                        Expression::SizeLiteral(literal.len())
                    ],
                ),
            );

            write!(out, "{};", init_expr)?;
        }

        Ok(())
    }

    fn add_class(&mut self, class_record: &semantic::RecordDecl, unit_scope: Rc<Scope>) -> TranslationResult<()> {
        let class = Class::translate(class_record, unit_scope)?;
        self.classes.push(class);
        Ok(())
    }

    fn add_decl(&mut self, decl: &semantic::UnitDecl, unit_scope: Rc<Scope>) -> TranslationResult<()> {
        let c_decls = Declaration::translate_decl(decl, self)?;

        match decl {
            UnitDecl::Type(TypeDecl::Record(class_record))
            if class_record.kind == RecordKind::Class => {
                self.add_class(class_record, unit_scope)?;
            }

            UnitDecl::Type(TypeDecl::Interface(interface_decl)) => {
                self.add_interface(interface_decl)?;
            }

            _ => {}
        }

        self.decls.extend(c_decls);
        Ok(())
    }

    fn add_interface(&mut self, iface_decl: &semantic::InterfaceDecl) -> TranslationResult<()> {
        let next_id = self.interfaces.len();

        let interface = Interface::translate(iface_decl, next_id)?;
        self.interfaces.push(interface);
        Ok(())
    }

    fn add_unit(&mut self, module_unit: &semantic::ModuleUnit) -> TranslationResult<()> {
        let unit = &module_unit.unit;

        for decl in unit.interface.iter() {
            self.add_decl(decl, module_unit.global_scope.clone())?;
        }

        for decl in unit.implementation.iter() {
            let new_impls = Declaration::translate_impl(decl, self)?;
            self.decls.extend(new_impls);
        }

        if let Some(init_block) = unit.initialization.as_ref() {
            let init_block = Block::translate(init_block, None, self)?;
            self.initialization.push(init_block);
        }

        if let Some(final_block) = unit.finalization.as_ref() {
            let final_block = Block::translate(final_block, None, self)?;
            self.finalization.push(final_block);
        }

        Ok(())
    }

    pub fn from_program(module: &semantic::ProgramModule) -> TranslationResult<Self> {
        let mut result = TranslationUnit {
            decls: Vec::new(),

            classes: Vec::new(),
            interfaces: Vec::new(),

            string_literals: LinkedHashSet::new(),

            initialization: Vec::new(),
            finalization: Vec::new(),
        };

        for (_, unit) in module.units.iter() {
            result.add_unit(unit)?;
        }

        for impl_decl in module.program.decls.iter() {
            result.add_implementation_decl(impl_decl, module.global_scope.clone())?;
        }

        /*
            have to do this in a second pass, because we don't know all the interface IDs
            before all decls are finished processing, and classes can be declared before
            the interfaces they implement
        */
        for class in result.classes.iter_mut() {
            class.update_vtables(&result.interfaces);
        }

        let global_vars: Vec<_> = module.units.values()
            .flat_map(|module_unit| module_unit.unit.vars())
            .chain(module.program.vars())
            .cloned()
            .collect();

        /* initialize the global var storing the ID of the System.Dispose interface for
        destructor lookups */
        let dispose_id = result.interfaces.iter()
            .find(|iface| iface.pascal_name == Identifier::from("System.Disposable"))
            .map(|iface| iface.id)
            .unwrap();

        result.initialization.push(Block::new(vec![
            Expression::binary_op(Name::internal_symbol("DisposeInterfaceID"),
                                  "=",
                                  Expression::SizeLiteral(dispose_id))
        ]));

        /* zero-initialize global variables */
        result.initialization.push(Block::new(global_vars.iter()
            .map(|var_decl: &semantic::VarDecl| {
                let name = Name::user_symbol(&var_decl.qualified_name());

                Expression::function_call(
                    Expression::Name(Name::internal_symbol("ZeroMemory")),
                    vec![
                        Expression::unary_op("&", name.clone(), true),
                        Expression::function_call(
                            Name::internal_symbol("SizeOf"),
                            vec![Expression::Name(name)]
                        )
                    ]
                )
            })
            .collect()));

        let main = Block::translate(&module.program.program_block, None, &mut result)?;
        result.initialization.push(main);
        result.initialization.push({
            let mut release_global_vars = Vec::new();
            // release all rc local vars for this block
            for decl in global_vars.iter().rev().filter(|decl| decl.decl_type.is_class()) {
                let name = Name::user_symbol(&decl.qualified_name());

                release_global_vars.push(rc_release(name));
            }

            Block::new(release_global_vars)
        });

        Ok(result)
    }

    pub fn declare_vtables(&self, out: &mut fmt::Write) -> fmt::Result {
        for iface in self.interfaces.iter() {
            iface.vtable.write_def(out)?;
        }

        for iface in self.interfaces.iter() {
            for method in iface.methods.values() {
                method.write_impl(out)?;
            }
        }

        for class in self.classes.iter() {
            class.write_decl(out)?;
        }

        Ok(())
    }

    pub fn method_call_name(&self, interface: &Identifier, method: &str) -> Option<&Name> {
        self.interfaces.iter()
            .find(|iface| iface.pascal_name == *interface)
            .and_then(|iface| iface.methods.get(method))
            .map(|method| &method.name)
    }

    pub fn write_internal_class_init(&self, out: &mut fmt::Write) -> fmt::Result {
        if let Some(string_class) = self.classes.iter().find(|c| c.is_internal()) {
            string_class.write_init(out)?;
        }

        Ok(())
    }

    pub fn write_class_init(&self, out: &mut fmt::Write) -> fmt::Result {
        for class in self.classes.iter().filter(|c| !c.is_internal()) {
            class.write_init(out)?;
        }

        Ok(())
    }
}
