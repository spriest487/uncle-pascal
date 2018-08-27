use std::{
    fmt,
    rc::Rc,
};
use linked_hash_map::{
    LinkedHashMap,
    Entry,
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
    FunctionDecl,
    FunctionArg,
    FunctionDefinition,
    CallingConvention,
    Interface,
    Expression,
    ExpressionContext,
    Name,
    Variable,
    CArray,
    CType,
    CastKind,
    Struct,
    StructDecl,
};
use types::{
    ParameterizedName,
    Type,
};
use semantic::{
    self,
    Scope,
    RecordDecl,
    Declaration as PascalDeclaration,
};

pub struct StructInstantiation {
    pub struct_decl: StructDecl,
    class: Option<Class>,
    type_args: Vec<Type>,
}

struct StructType {
    pascal_decl: RecordDecl,
    instantiations: LinkedHashMap<Vec<Type>, StructInstantiation>,
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct ArrayCtorKey {
    element: Type,
    size: usize,
}

pub struct TranslationUnit {
    decls: Vec<Declaration>,

    interfaces: Vec<Interface>,
    string_literals: LinkedHashSet<String>,

    structs: LinkedHashMap<Identifier, StructType>,

    array_ctors: LinkedHashMap<ArrayCtorKey, FunctionDecl>,

    initialization: Vec<Block>,
    finalization: Vec<Block>,
}

impl TranslationUnit {
    pub fn initialization(&self) -> &[Block] {
        &self.initialization
    }

    pub fn finalization(&self) -> &[Block] {
        &self.finalization
    }

    fn add_implementation_decl(&mut self, decl: &semantic::Implementation) -> TranslationResult<()> {
        match decl {
            Implementation::Decl(decl) => {
                self.add_decl(decl)?;
            }
            _ => {
                let impl_decl = Declaration::translate_impl(decl, self)?;
                self.decls.extend(impl_decl);
            }
        }

        Ok(())
    }

    pub fn array_ctor(&mut self, element: &Type, elements: Vec<Expression>, scope: &Scope) -> TranslationResult<Expression> {
        let key = ArrayCtorKey {
            element: element.clone(),
            size: elements.len(),
        };

        let element_ctype = CType::translate(&element, scope, self)?;

        let ctor_name = match self.array_ctors.entry(key) {
            Entry::Occupied(entry) => {
                entry.get().name.clone()
            }

            Entry::Vacant(entry) => {
                let array_ctype = CType::Array(CArray {
                    element: Box::new(element_ctype.clone()),
                    count: elements.len(),
                });

                let arg_names: Vec<_> = (0..elements.len()).map(|i| Name::local(format!("e{}", i)))
                    .collect();

                let result_name = Name::local("result");

                let body = {
                    let mut statements = Vec::new();
                    statements.push(Expression::local_decl(array_ctype.clone(), result_name.clone(), None));

                    let elements = Expression::member(result_name.clone(), "Elements");

                    for (i, arg_name) in arg_names.iter().enumerate() {
                        let index = Expression::SizeLiteral(i);
                        statements.push(Expression::binary_op(
                            Expression::array_element(elements.clone(), index),
                            "=",
                            arg_name.clone(),
                        ));
                    }
                    statements.push(Expression::return_value(result_name));
                    Block::new(statements)
                };

                let ctor_func = FunctionDecl {
                    name: Name::array_constructor(&element, elements.len()),
                    args: arg_names.into_iter().map(|name| FunctionArg {
                        name,
                        ctype: element_ctype.clone(),
                    }).collect(),
                    calling_convention: CallingConvention::Cdecl,
                    return_type: array_ctype,
                    definition: FunctionDefinition::Defined(body),
                };

                entry.insert(ctor_func).name.clone()
            }
        };

        Ok(Expression::function_call(ctor_name, elements))
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

    fn declare_string_literals(&self, out: &mut fmt::Write) -> fmt::Result {
        for (index, _) in self.string_literals.iter().enumerate() {
            let name = Self::string_literal_index_to_name(index);

            let var = Variable {
                name,
                ctype: CType::Struct(Name::user_type(&ParameterizedName::new_simple("System.String")))
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

            let byte_type = CType::Named(Name::user_type(&ParameterizedName::new_simple("System.Byte")));

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
                name.clone(),
                "=",
                Expression::function_call(
                    Name::user_symbol(&Identifier::from("System.StringFromBytes")),
                    vec![
                        literal_as_mut_bytes,
                        Expression::SizeLiteral(literal.len())
                    ],
                ),
            );

            writeln!(out, "{};", init_expr)?;
        }

        Ok(())
    }

    pub fn declare_struct(&mut self, decl: RecordDecl) {
        self.structs.entry(decl.qualified_name()).or_insert_with(|| {
            StructType {
                pascal_decl: decl,
                instantiations: LinkedHashMap::new(),
            }
        });
    }

    fn instantiate_struct_type(&mut self,
                               name: &ParameterizedName)
                               -> TranslationResult<()> {
        let pascal_decl = self.structs[&name.name].pascal_decl
            .clone()
            .specialize(&name.type_args);
        let decl = Struct::translate(&pascal_decl, &name.type_args, self)?;

        self.structs[&name.name].instantiations.insert(
            name.type_args.clone(),
            StructInstantiation {
                struct_decl: decl,

                /* leave this blank for now, we'll analyze which structs belong to classes in a
                second pass */
                class: None,

                type_args: name.type_args.clone(),
            },
        );

        Ok(())
    }

    pub fn struct_instantiations(&self, base_name: &Identifier) -> impl Iterator<Item=&StructInstantiation> {
        self.structs[base_name].instantiations.values()
    }

    pub fn struct_decl(&mut self, name: &ParameterizedName) -> TranslationResult<&StructDecl> {
        if !self.structs[&name.name].instantiations.contains_key(&name.type_args) {
            self.instantiate_struct_type(name)?;
        }

        Ok(&self.structs[&name.name]
            .instantiations[&name.type_args]
            .struct_decl)
    }

    fn add_decl(&mut self, decl: &semantic::UnitDecl) -> TranslationResult<()> {
        let c_decls = Declaration::translate_decl(decl, self)?;

        if let UnitDecl::Type(TypeDecl::Interface(interface_decl)) = decl {
            self.add_interface(interface_decl)?;
        }

        self.decls.extend(c_decls);
        Ok(())
    }

    fn add_interface(&mut self, iface_decl: &semantic::InterfaceDecl) -> TranslationResult<()> {
        let next_id = self.interfaces.len();

        let interface = Interface::translate(iface_decl, self, next_id)?;
        self.interfaces.push(interface);
        Ok(())
    }

    fn add_unit(&mut self, module_unit: &semantic::ModuleUnit) -> TranslationResult<()> {
        let unit = &module_unit.unit;

        for decl in &unit.interface {
            self.add_decl(decl)?;
        }

        for decl in &unit.implementation {
            let new_impls = Declaration::translate_impl(decl, self)?;
            self.decls.extend(new_impls);
        }

        if let Some(init_block) = unit.initialization.as_ref() {
            let init_block = Block::translate(init_block, None, &mut ExpressionContext::root(self))?;
            self.initialization.push(init_block);
        }

        if let Some(final_block) = unit.finalization.as_ref() {
            let final_block = Block::translate(final_block, None, &mut ExpressionContext::root(self))?;
            self.finalization.push(final_block);
        }

        Ok(())
    }

    pub fn new() -> Self {
        TranslationUnit {
            decls: Vec::new(),

            interfaces: Vec::new(),

            string_literals: LinkedHashSet::new(),

            array_ctors: LinkedHashMap::new(),

            initialization: Vec::new(),
            finalization: Vec::new(),

            structs: LinkedHashMap::new(),
        }
    }

    pub fn translate_program(module: &semantic::ProgramModule) -> TranslationResult<Self> {
        let mut result = TranslationUnit::new();

        for (_, unit) in &module.units {
            result.add_unit(unit)?;
        }

        for impl_decl in &module.program.decls {
            result.add_implementation_decl(impl_decl)?;
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
                            vec![Expression::Name(name)],
                        )
                    ],
                )
            })
            .collect()));

        let main = Block::translate(
            &module.program.program_block,
            None,
            &mut ExpressionContext::root(&mut result),
        )?;
        result.initialization.push(main);
        result.initialization.push({
            let mut release_global_vars = Vec::new();
            // release all rc local vars for this block
            for decl in global_vars.iter().rev().filter(|decl| decl.decl_type.is_class_ref()) {
                let name = Name::user_symbol(&decl.qualified_name());

                release_global_vars.push(rc_release(name));
            }

            Block::new(release_global_vars)
        });

        /*
            have to do this in a second pass, because we don't know all the interface IDs
            before all decls are finished processing, and classes can be declared before
            the interfaces they implement

            this has to come after the body, in case there are types or specializations only
            referenced in the program body
        */
        result.discover_classes(&module.global_scope)?;

        Ok(result)
    }

    fn classes(&self) -> impl Iterator<Item=&Class> {
        self.structs.values()
            .flat_map(|struct_type| struct_type.instantiations.values())
            .filter_map(|struct_type| struct_type.class.as_ref())
    }

    fn discover_classes(&mut self, scope: &Rc<Scope>) -> TranslationResult<()> {
        let class_structs = self.structs.iter_mut()
            .filter_map(|(_, class_struct)| match class_struct.pascal_decl.kind {
                RecordKind::Class => Some(class_struct),
                RecordKind::Record => None,
            });

        for class_struct in class_structs {
            for (_, mut instantiation) in &mut class_struct.instantiations {
                let mut class = Class::translate(
                    &class_struct.pascal_decl,
                    &instantiation.type_args,
                    &scope.clone(),
                )?;
                class.update_vtables(&self.interfaces);

                instantiation.class = Some(class);
            }
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
        if let Some(string_class) = self.classes().find(|c| c.is_internal()) {
            string_class.write_init(out)?;
        }

        Ok(())
    }

    pub fn write_class_init(&self, out: &mut fmt::Write) -> fmt::Result {
        for class in self.classes().filter(|c| !c.is_internal()) {
            class.write_init(out)?;
        }

        Ok(())
    }

    pub fn write_decls(&self, out: &mut fmt::Write) -> fmt::Result {
        self.declare_string_literals(out)?;

        // declare generated array constructors
        for ctor in self.array_ctors.values() {
            ctor.write_forward(out)?;
        }

        // declare interface vtables and vtable impl funcs
        for iface in &self.interfaces {
            iface.vtable.write_def(out)?;
        }

        for iface in &self.interfaces {
            for method in iface.methods.values() {
                method.write_impl(out)?;
            }
        }

        // forward-declare user functions
        for decl in &self.decls {
            decl.write_forward(self, out)?;
        }

        // write class func decls
        for class in self.classes() {
            class.write_decl(out)?;
        }

        // write generated array constructors
        for ctor in self.array_ctors.values() {
            ctor.write_impl(out)?;
        }

        // write user function impls
        for decl_impl in &self.decls {
            decl_impl.write_impl(self, out)?;
        }

        // write class func impls
        for class in self.classes() {
            class.write_impl(out)?;
        }

        Ok(())
    }
}
