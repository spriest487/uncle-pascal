use crate::write_instruction_list;
use crate::ir;
use common::span::Span;
use std::collections::HashMap;
use std::fmt;
use ir_lang::InstructionFormatter;
use ir_lang::Metadata;

#[derive(Clone, Debug)]
pub struct Module {
    pub(crate) metadata: Metadata,

    pub(crate) functions: HashMap<ir::FunctionID, ir::Function>,

    pub(crate) static_closures: Vec<ir::StaticClosure>,
    pub(crate) function_static_closures: HashMap<ir::FunctionID, ir::StaticClosureID>,

    pub(crate) init: Vec<ir::Instruction>,
    
    pub(crate) span: Option<Span>,
}

impl Module {
    pub fn new(metadata: Metadata) -> Self {
        let module = Self {
            init: Vec::new(),

            functions: HashMap::new(),

            static_closures: Vec::new(),
            function_static_closures: HashMap::new(),
            
            span: None,

            metadata,
        };

        module
    }

    pub fn closure_types(&self) -> impl Iterator<Item = ir::TypeDefID> + '_ {
        self.metadata.closures().iter().cloned()
    }

    pub fn static_closures(&self) -> &[ir::StaticClosure] {
        &self.static_closures
    }

    pub fn find_dyn_array_struct(&self, elem_ty: &ir::Type) -> Option<ir::TypeDefID> {
        self.metadata.find_dyn_array_struct(elem_ty)
    }
    
    pub fn span(&self) -> Option<&Span> {
        self.span.as_ref()
    }
    
    pub fn init(&self) -> &[ir::Instruction] {
        self.init.as_slice()
    }
    
    pub fn metadata(&self) -> &Metadata {
        &self.metadata
    }
    
    pub fn functions(&self) -> &HashMap<ir::FunctionID, ir::Function> {
        &self.functions
    }
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "* Type Definitions")?;
        let mut defs: Vec<_> = self.metadata.type_defs().collect();
        defs.sort_by_key(|(id, _)| *id);

        for (id, def) in &defs {
            match def {
                ir::TypeDef::Struct(s) => {
                    write!(f, "{}: ", id.0)?;

                    match &s.identity {
                        ir::StructIdentity::Class(name) | ir::StructIdentity::Record(name) => {
                            self.metadata.format_name(name, f)?;
                        },

                        ir::StructIdentity::Closure(identity) => {
                            let func_ty_name = self
                                .metadata
                                .pretty_ty_name(&ir::Type::Function(identity.virt_func_ty));
                            write!(
                                f,
                                "closure of {} @ {}:{}:{}",
                                func_ty_name, identity.module, identity.line, identity.col
                            )?;
                        },

                        ir::StructIdentity::Array(element, dim) => {
                            write!(f, "array[{dim}] of {}", self.metadata.pretty_ty_name(element))?;
                        }

                        ir::StructIdentity::DynArray(element) => {
                            write!(f, "array of {}", self.metadata.pretty_ty_name(element))?;
                        }
                    }

                    writeln!(f)?;

                    let max_field_id = s.fields.keys().max().cloned().unwrap_or(ir::FieldID(0));
                    let fields = (0..=max_field_id.0).filter_map(|id| {
                        let field = s.fields.get(&ir::FieldID(id))?;
                        Some((id, field))
                    });

                    for (id, field) in fields {
                        write!(f, "  {:8>}: ", id)?;
                        self.metadata.format_type(&field.ty, f)?;

                        if let Some(field_name) = &field.name {
                            write!(f, " (`{}`)", field_name)?;
                        }

                        writeln!(f)?;
                    }

                    let ty_as_struct = ir::Type::Struct(*id);
                    let ty_as_class = ir::Type::RcPointer(ir::VirtualTypeID::Class(*id));
                    let mut iface_impls = self.metadata.impls(&ty_as_struct);
                    iface_impls.extend(self.metadata.impls(&ty_as_class));

                    if !iface_impls.is_empty() {
                        writeln!(f, "  Implements:")?;
                        for iface_id in iface_impls {
                            writeln!(f, "    {}", self.metadata.iface_name(iface_id))?;
                        }
                    }
                },

                ir::TypeDef::Variant(v) => {
                    writeln!(f, "{}: {}", id, v.name)?;
                    for (i, case) in v.cases.iter().enumerate() {
                        write!(f, "{:8>} ({})", format!("  .{}", i), case.name,)?;

                        if let Some(ty) = &case.ty {
                            write!(f, ": {}", ty)?;
                        }
                        writeln!(f)?;
                    }
                },

                ir::TypeDef::Function(def) => {
                    write!(f, "{}: {}", id.0, self.metadata.pretty_func_sig(def))?;
                },
            }

            writeln!(f)?;
        }

        writeln!(f, "* Interfaces: ")?;
        let mut ifaces: Vec<_> = self.metadata.ifaces().collect();
        ifaces.sort_by_key(|(id, _)| *id);

        for (id, iface) in &ifaces {
            writeln!(f, "{}: {}", id, iface.name)?;

            for (i, method) in iface.methods.iter().enumerate() {
                let sig_params: Vec<_> = method
                    .params
                    .iter()
                    .map(|param| self.metadata.pretty_ty_name(param))
                    .collect();
                let return_ty = self.metadata.pretty_ty_name(&method.return_ty);

                let sig = format!("({}) -> {}", sig_params.join(", "), return_ty);

                let index = format!("  .{}", i);
                write!(f, "{:8>} ({}): {}", index, method.name, sig)?;
            }
            writeln!(f)?;
        }
        writeln!(f)?;

        writeln!(f, "* String literals")?;
        for (id, lit) in self.metadata.strings() {
            writeln!(f, "{}: '{}'", id.0, lit)?;
        }
        writeln!(f)?;

        let mut funcs: Vec<_> = self.functions.iter().collect();
        funcs.sort_by_key(|(id, _)| **id);

        writeln!(f, "* Functions")?;
        for (id, func) in funcs {
            write!(f, "{}: ", id.0)?;
            match self.metadata.func_desc(*id) {
                Some(desc_name) => {
                    writeln!(f, "{}", desc_name)?;
                },
                None => {
                    writeln!(f, " /* {} */", func.debug_name())?;
                },
            }

            match func {
                ir::Function::Local(ir::FunctionDef { body, .. }) => {
                    write_instruction_list(f, &self.metadata, body)?;
                },

                ir::Function::External(ir::ExternalFunctionRef { symbol, src, .. }) => {
                    writeln!(f, "<external function '{}' in module '{}'>", symbol, src)?;
                },
            }
            writeln!(f)?;
        }

        writeln!(f, "* Init:")?;
        write_instruction_list(f, &self.metadata, &self.init)?;
        Ok(())
    }
}
