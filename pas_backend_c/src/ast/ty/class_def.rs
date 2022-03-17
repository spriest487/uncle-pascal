use crate::ast::{
    Expr, FieldName, FunctionDecl, FunctionDef, FunctionName, GlobalName, Module, Statement, Type,
    TypeDefName,
};
use pas_ir::{
    metadata::{
        self, FunctionID, InterfaceID, MethodID, RcBoilerplatePair, TypeDefID, VirtualTypeID,
        DISPOSABLE_DISPOSE_INDEX, DISPOSABLE_ID, DYNARRAY_LEN_FIELD, DYNARRAY_PTR_FIELD,
    },
    LocalID,
};
use std::{
    collections::{BTreeMap, HashMap},
    fmt::Write,
};

#[derive(Clone, Debug)]
struct MethodImplFunc {
    name: FunctionName,

    vcall_wrapper_decl: FunctionDecl,
}

impl MethodImplFunc {
    fn new(
        iface_id: InterfaceID,
        self_ty_id: TypeDefID,
        method_id: MethodID,
        iface_method: &pas_ir::metadata::Method,
        impl_func_id: FunctionID,
        metadata: &metadata::Metadata,
        module: &mut Module,
    ) -> Self {
        let class_ty = pas_ir::Type::RcPointer(VirtualTypeID::Class(self_ty_id));
        let iface_ty = pas_ir::Type::RcPointer(VirtualTypeID::Interface(iface_id));

        let impl_func_name = FunctionName::ID(impl_func_id);
        let vcall_wrapper_name = FunctionName::MethodWrapper(iface_id, method_id, self_ty_id);

        // generate virtual call wrapper with the param types of the virtually called iface method
        let wrapper_param_tys: Vec<_> = iface_method
            .params
            .iter()
            .map(|ty| Type::from_metadata(ty, module))
            .collect();
        let wrapper_return_ty = Type::from_metadata(&iface_method.return_ty, module);

        Self {
            name: impl_func_name,
            vcall_wrapper_decl: FunctionDecl {
                comment: Some(format!(
                    "virtual call wrapper impl of {}.{} for {}",
                    metadata.pretty_ty_name(&iface_ty),
                    iface_method.name,
                    metadata.pretty_ty_name(&class_ty),
                )),
                name: vcall_wrapper_name,
                params: wrapper_param_tys,
                return_ty: wrapper_return_ty,
            },
        }
    }

    fn gen_vcall_wrapper(&self, module: &Module) -> FunctionDef {
        let mut next_param_local = LocalID(match &self.vcall_wrapper_decl.return_ty {
            Type::Void => 0,
            _ => 1,
        });

        let impl_func_def = module
            .functions
            .iter()
            .find(|f| f.decl.name == self.name)
            .unwrap();

        let mut call_impl_func_args = Vec::with_capacity(self.vcall_wrapper_decl.params.len());

        // forward the rest of the params as-is
        for i in 0..self.vcall_wrapper_decl.params.len() {
            let concrete_ty = &impl_func_def.decl.params[i];

            call_impl_func_args.push(Expr::Local(next_param_local).cast(concrete_ty.clone()));
            next_param_local.0 += 1;
        }

        let call_impl_func = Expr::call(Expr::Function(self.name), call_impl_func_args);

        let body_stmt = match impl_func_def.decl.return_ty {
            Type::Void => Statement::Expr(call_impl_func),

            _ => {
                let result_expr = Expr::Local(LocalID(0));
                let casted_result = call_impl_func.cast(self.vcall_wrapper_decl.return_ty.clone());
                Statement::Expr(Expr::assign(result_expr, casted_result))
            },
        };

        FunctionDef {
            decl: self.vcall_wrapper_decl.clone(),
            body: vec![body_stmt],
        }
    }
}

#[derive(Clone, Debug)]
pub struct InterfaceImpl {
    method_impls: BTreeMap<MethodID, MethodImplFunc>,
}

#[derive(Clone, Debug)]
struct DynArrayTypeInfo {
    el_ty: Type,
    el_rc: bool,
    el_type_info: Option<RcBoilerplatePair>,
}

#[derive(Clone, Debug)]
pub struct Class {
    struct_id: TypeDefID,
    impls: HashMap<InterfaceID, InterfaceImpl>,

    disposer: Option<MethodImplFunc>,
    cleanup_func: FunctionName,

    // if this class is a dyn array, the RTTI for it
    dyn_array_type_info: Option<DynArrayTypeInfo>,
}

impl Class {
    pub fn translate(
        struct_id: TypeDefID,
        _struct_def: &metadata::Struct,
        metadata: &metadata::Metadata,
        module: &mut Module,
    ) -> Self {
        let class_ty = pas_ir::Type::RcPointer(VirtualTypeID::Class(struct_id));

        let mut impls = HashMap::new();

        for (iface_id, iface_impl) in metadata.find_impls(&class_ty) {
            let mut method_impls = BTreeMap::new();

            let iface = metadata.get_iface_def(iface_id).unwrap();

            for (method_id, impl_func_id) in iface_impl.methods.iter() {
                let method_def = iface.get_method(*method_id).unwrap();

                let impl_func = MethodImplFunc::new(
                    iface_id,
                    struct_id,
                    *method_id,
                    method_def,
                    *impl_func_id,
                    metadata,
                    module,
                );

                method_impls.insert(*method_id, impl_func);
            }

            if method_impls.is_empty() {
                continue;
            }

            impls.insert(iface_id, InterfaceImpl { method_impls });
        }

        let disposer = impls
            .get(&metadata::DISPOSABLE_ID)
            .and_then(|disposable_impl| {
                disposable_impl
                    .method_impls
                    .get(&metadata::DISPOSABLE_DISPOSE_INDEX)
                    .cloned()
            });

        let resource_ty = metadata::Type::Struct(struct_id);
        let cleanup_func = metadata
            .find_rc_boilerplate(&resource_ty)
            .map(|funcs| FunctionName::ID(funcs.release))
            .unwrap_or_else(|| {
                panic!(
                    "missing rc cleanup func for resource struct of IR class {}",
                    metadata.pretty_ty_name(&resource_ty),
                )
            });

        let dyn_array_type_info = metadata
            .dyn_array_structs()
            .iter()
            .filter_map(|(el_ty, arr_struct_id)| {
                if *arr_struct_id == struct_id {
                    let el_rc = el_ty.is_rc();
                    let el_type_info = metadata.find_rc_boilerplate(&el_ty);
                    let el_ty = Type::from_metadata(el_ty, module);

                    Some(DynArrayTypeInfo {
                        el_type_info,
                        el_ty,
                        el_rc,
                    })
                } else {
                    None
                }
            })
            .next();

        Class {
            struct_id,
            impls,
            disposer,
            cleanup_func,
            dyn_array_type_info,
        }
    }

    pub fn gen_vcall_wrappers(&self, module: &Module) -> Vec<FunctionDef> {
        let mut wrappers = Vec::new();
        for (_iface_id, iface_impl) in &self.impls {
            for (_method_id, method_impl) in &iface_impl.method_impls {
                wrappers.push(method_impl.gen_vcall_wrapper(module));
            }
        }

        wrappers
    }

    pub fn to_decl_string(&self) -> String {
        let mut decls = String::new();
        for (iface_id, _) in &self.impls {
            decls
                .write_fmt(format_args!(
                    "struct MethodTable_{} ImplTable_{}_{};\n",
                    iface_id.0, self.struct_id.0, iface_id.0
                ))
                .unwrap();
        }

        match self.dyn_array_type_info {
            Some(..) => {
                writeln!(
                    decls,
                    "struct DynArrayClass {};",
                    GlobalName::ClassInstance(self.struct_id)
                )
                .unwrap();
            },
            None => {
                writeln!(
                    decls,
                    "struct Class {};",
                    GlobalName::ClassInstance(self.struct_id)
                )
                .unwrap();
            },
        }

        decls
    }

    pub fn to_def_string(&self) -> String {
        let mut def = String::new();

        // impl method tables
        let impls: Vec<_> = self.impls.iter().enumerate().collect();

        for (i, (iface_id, iface_impl)) in &impls {
            def.push_str(&format!(
                "struct MethodTable_{} ImplTable_{}_{} = {{\n",
                iface_id.0, self.struct_id.0, iface_id.0
            ));
            for (method_id, _method_name) in &iface_impl.method_impls {
                let wrapper_name =
                    FunctionName::MethodWrapper(**iface_id, *method_id, self.struct_id);

                def.push_str("  .base = {\n");

                def.push_str("    .iface = ");
                def.push_str(&iface_id.to_string());
                def.push_str(",\n");

                def.push_str("    .next = ");
                match impls.get(i + 1) {
                    Some((_, (next_impl_id, _))) => {
                        def.push_str(&format!(
                            "&ImplTable_{}_{}.base",
                            self.struct_id, next_impl_id
                        ));
                    },
                    None => def.push_str("NULL"),
                }

                def.push_str(",\n");
                def.push_str("  },\n");

                def.push_str("  .method_");
                def.push_str(&method_id.0.to_string());
                def.push_str(" = &");
                def.push_str(&wrapper_name.to_string());
                def.push_str(",\n");
            }
            def.push_str("};\n\n");
        }

        // class struct

        let mut class_init = String::new();
        writeln!(class_init, "{{").unwrap();
        writeln!(
            class_init,
            "  .size = sizeof(struct {}),",
            TypeDefName::Struct(self.struct_id)
        )
        .unwrap();

        if let Some(..) = &self.disposer {
            let dispose_wrapper = FunctionName::MethodWrapper(
                DISPOSABLE_ID,
                DISPOSABLE_DISPOSE_INDEX,
                self.struct_id,
            );
            writeln!(class_init, "  .disposer = &{},", dispose_wrapper).unwrap();
        } else {
            writeln!(class_init, "  .disposer = NULL,").unwrap();
        };

        writeln!(
            class_init,
            "  .cleanup = (RcCleanupFunc) &{},",
            self.cleanup_func
        )
        .unwrap();

        if let Some((_, (first_iface_id, _))) = impls.get(0) {
            writeln!(
                class_init,
                "  .iface_methods = (struct MethodTable*) &ImplTable_{}_{},",
                self.struct_id, first_iface_id
            )
            .unwrap();
        } else {
            writeln!(class_init, "  .iface_methods = NULL,").unwrap();
        }

        class_init.push_str("}");

        match &self.dyn_array_type_info {
            Some(dyn_array_ty_info) => {
                let el_ty = dyn_array_ty_info.el_ty.clone();

                let el_retain_func = dyn_array_ty_info
                    .el_type_info
                    .as_ref()
                    .map(|type_info| FunctionName::ID(type_info.retain));

                let retain_el_proc = if dyn_array_ty_info.el_rc {
                    format!("{}(*el_ptr);", FunctionName::RcRetain)
                } else if let Some(el_retain_func) = el_retain_func {
                    format!("{}(el_ptr);", el_retain_func)
                } else {
                    String::new()
                };

                writeln!(
                    def,
                    r#"
                void DynArrayAlloc_{struct_id}(struct Rc* arr, int32_t len, struct Rc* copy_from, void* default_val, int32_t default_val_len) {{
                    if (!copy_from || !copy_from->resource) {{
                        abort();
                    }}

                    {res_ty}* copy_arr_res = ({res_ty}*) copy_from->resource;
                    int32_t copy_len = copy_arr_res->{len_field};

                    int32_t data_len = (int32_t) sizeof({el_ty}) * len;
                    {el_ty}* data = ({el_ty}*) System_GetMem(data_len);

                    if (len > 0) {{
                        int32_t copy_count = len < copy_len ? len : copy_len;
                        size_t copy_size = (size_t) copy_count * sizeof({el_ty});

                        memcpy(data, copy_arr_res->{data_field}, copy_size);
                    }}

                    for (int32_t i = 0; i < len; i += 1) {{
                        {el_ty}* el_ptr = &data[i];

                        if (i >= copy_len) {{
                            memcpy(el_ptr, default_val, default_val_len);
                        }}

                        {retain_el_proc}
                    }}

                    {res_ty}* arr_resource = ({res_ty}*) arr->resource;
                    arr_resource->{len_field} = len;
                    arr_resource->{data_field} = data;
                }}

                int32_t DynArrayLength_{struct_id}(struct Rc* arr) {{
                    if (!arr || !arr->resource) {{
                        abort();
                    }}

                    {res_ty}* arr_resource = ({res_ty}*) arr->resource;
                    return arr_resource->{len_field};
                }}

                struct DynArrayClass {class_name} = {{
                    .base = {class_init},
                    .alloc = DynArrayAlloc_{struct_id},
                    .length = DynArrayLength_{struct_id},
                }};"#,
                    struct_id = self.struct_id,
                    class_name = GlobalName::ClassInstance(self.struct_id),
                    res_ty = Type::DefinedType(TypeDefName::Struct(self.struct_id)).typename(),
                    el_ty = el_ty.typename(),
                    retain_el_proc = retain_el_proc,
                    class_init = class_init,
                    len_field = FieldName::ID(DYNARRAY_LEN_FIELD),
                    data_field = FieldName::ID(DYNARRAY_PTR_FIELD),
                ).unwrap();
            },

            None => {
                writeln!(
                    def,
                    "struct Class Class_{} = {};",
                    self.struct_id, class_init,
                )
                .unwrap();
            },
        }

        def
    }
}

pub struct Interface {
    id: InterfaceID,
    methods: Vec<FunctionDecl>,
}

impl Interface {
    pub fn translate(
        iface_id: InterfaceID,
        iface: &metadata::Interface,
        module: &mut Module,
    ) -> Self {
        let methods = iface
            .methods
            .iter()
            .enumerate()
            .map(|(method_index, method)| {
                let return_ty = Type::from_metadata(&method.return_ty, module);
                let method_id = MethodID(method_index);
                let params = method
                    .params
                    .iter()
                    .map(|param| Type::from_metadata(param, module))
                    .collect();

                let name = FunctionName::Method(iface_id, method_id);

                let comment = Some(format!(
                    "Method {} of interface {}",
                    method.name, iface.name
                ));

                FunctionDecl {
                    return_ty,
                    params,
                    name,
                    comment,
                }
            })
            .collect();

        Self {
            id: iface_id,
            methods,
        }
    }

    pub fn method_table_string(&self) -> String {
        let mut table = format!("struct MethodTable_{} {{\n", self.id.0);
        table.push_str("  struct MethodTable base;\n");

        for (method_index, method) in self.methods.iter().enumerate() {
            table.push_str("  ");
            let method_ptr_name = format!("method_{}", method_index);
            table.push_str(&method.ptr_type().to_decl_string(&method_ptr_name));
            table.push_str(";\n");
        }

        table.push_str("};\n\n");

        // vcall thunks for each method
        for (method_index, method) in self.methods.iter().enumerate() {
            table.push_str(&method.to_string());
            table.push_str(" {\n");

            let (self_arg_local, arg_offset) = match method.return_ty {
                Type::Void => (Expr::Local(LocalID(0)), 0),
                _ => (Expr::Local(LocalID(1)), 1),
            };

            let self_arg_rc = self_arg_local.cast(Type::Rc.ptr());

            // find the matching table
            table.push_str(&format!(
                "  struct MethodTable* table = {}->class->iface_methods;\n",
                self_arg_rc
            ));
            table.push_str("  while (table) {\n");
            table.push_str(&format!("    if (table->iface == {}) {{\n", self.id.0));
            table.push_str(&format!(
                "      struct MethodTable_{}* my_table = (struct MethodTable_{}*) table;\n",
                self.id.0, self.id.0
            ));

            // get the pointer from the table
            table.push_str("      ");
            table.push_str(&method.ptr_type().to_decl_string("method_ptr"));
            table.push_str(&format!(" = my_table->method_{};\n", method_index));

            if method.return_ty == Type::Void {
                table.push_str("      ");
            } else {
                table.push_str("      return ");
            }

            // call the function
            table.push_str("method_ptr(");

            for (i, _param) in method.params.iter().enumerate() {
                if i > 0 {
                    table.push_str(", ");
                }
                table.push_str("L");
                table.push_str(&(i + arg_offset).to_string());
            }
            table.push_str(");\n");

            if method.return_ty == Type::Void {
                table.push_str("      return;\n");
            }

            table.push_str("    } else {\n");
            table.push_str("      table = table->next;\n");
            table.push_str("    }\n");

            table.push_str("  }\n");

            // missing vcall
            table.push_str("  abort();\n");

            table.push_str("}\n\n");
        }
        table
    }
}
