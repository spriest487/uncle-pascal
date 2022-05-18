use std::fmt;

pub use self::{formatter::*, instruction::*, metadata::ty::Type, module::*, val::*, function::*};
use crate::ty::{VirtualTypeID, FieldID, TypeDef};
use crate::{builder::Builder, expr::*, metadata::*, stmt::*};
use pas_syn::IdentPath;
use pas_typecheck as pas_ty;

mod builder;
mod dep_sort;
mod expr;
mod formatter;
mod instruction;
pub mod metadata;
mod module;
mod stmt;
mod val;
mod pattern;
mod function;

pub const RETURN_REF: Ref = Ref::Local(LocalID(0));
pub const EXIT_LABEL: Label = Label(0);

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct IROptions {
    // insert IR comments indicating scoped lifetimes
    pub annotate_scopes: bool,

    // insert IR comments indicating RC release/retain operations
    pub annotate_rc: bool,

    // insert source spans for statements and expressions for improved error messaging in the
    // translation/interpreter stage
    pub debug_info: bool,
}

impl Default for IROptions {
    fn default() -> Self {
        Self {
            annotate_scopes: false,
            annotate_rc: false,

            debug_info: true,
        }
    }
}

fn write_instruction_list(
    f: &mut fmt::Formatter,
    metadata: &Metadata,
    instructions: &[Instruction],
) -> fmt::Result {
    let num_len = instructions.len().to_string().len();

    let formatter = StatefulIndentedFormatter::new(metadata, 4);

    for (i, instruction) in instructions.iter().enumerate() {
        write!(f, "{:>width$}|", i, width = num_len)?;
        formatter.format_instruction(instruction, f)?;
        writeln!(f)?;
    }

    Ok(())
}

fn gen_dyn_array_funcs(module: &mut Module, elem_ty: &Type, struct_id: TypeDefID) {
    let mut alloc_builder = Builder::new(module);
    gen_dyn_array_alloc_func(&mut alloc_builder, elem_ty, struct_id);
    let alloc_body = alloc_builder.finish();

    let dyn_array_rtti = module.metadata.get_dynarray_runtime_type(elem_ty)
        .expect("missing dynarray rtti for type");

    module.insert_func(dyn_array_rtti.alloc, Function::Local(FunctionDef {
        debug_name: format!("dynarray alloc function for element type {}", elem_ty),
        params: vec![Type::any(), Type::I32, Type::any(), Type::any()],
        return_ty: Type::Nothing,
        body: alloc_body,
        src_span: module.module_span().clone(),
    }));

    let mut length_builder = Builder::new(module);
    gen_dyn_array_length_func(&mut length_builder, struct_id);
    let length_body = length_builder.finish();

    module.insert_func(dyn_array_rtti.length, Function::Local(FunctionDef {
        debug_name: format!("dynarray length function for element type {}", elem_ty),
        params: vec![Type::any()],
        return_ty: Type::I32,
        body: length_body,
        src_span: module.module_span().clone(),
    }));
}

fn gen_dyn_array_alloc_func(builder: &mut Builder, elem_ty: &Type, struct_id: TypeDefID) {
    let array_ref_ty = Type::RcPointer(VirtualTypeID::Class(struct_id));
    let el_ptr_ty = elem_ty.clone().ptr();

    // bind params
    let arr_arg = LocalID(0);
    let len_arg = LocalID(1);
    let src_arr_arg = LocalID(2);
    let default_val_arg = LocalID(3);
    builder.bind_param(arr_arg, Type::any(), "arr_ptr", false);
    builder.bind_param(len_arg, Type::I32, "len", false);
    builder.bind_param(src_arr_arg, Type::any(), "src_arr_ptr", false);
    builder.bind_param(default_val_arg, Type::Nothing.ptr(), "default_val", false);

    // retain the refs to the array params
    builder.retain(Ref::Local(LocalID(0)), &Type::any());
    builder.retain(Ref::Local(LocalID(2)), &Type::any());

    // cast the array params to this array type
    let arr = builder.local_temp(array_ref_ty.clone());
    let src_arr = builder.local_temp(array_ref_ty.clone());
    builder.cast(arr.clone(), Ref::Local(LocalID(0)), array_ref_ty.clone());
    builder.cast(src_arr.clone(), Ref::Local(LocalID(2)), array_ref_ty.clone());

    let default_el_ptr = builder.local_temp(el_ptr_ty.clone());
    builder.cast(default_el_ptr.clone(), default_val_arg, el_ptr_ty.clone());

    // copy_len := copy_from->length
    let src_len = builder.local_temp(Type::I32);
    builder.field_val(src_len.clone(), src_arr.clone(), array_ref_ty.clone(), DYNARRAY_LEN_FIELD, Type::I32);

    // el_len := sizeof(elem_ty)
    let el_len = builder.local_temp(Type::I32);
    builder.size_of(el_len.clone(), elem_ty.clone());

    // data_len := el_len * len
    let data_len = builder.local_temp(Type::I32);
    builder.mul(data_len.clone(), el_len.clone(), len_arg);

    // data = GetMem(data_len) as ^elem_ty
    let data_mem = builder.local_temp(Type::U8.ptr());
    builder.get_mem(data_len, data_mem.clone());
    let data = builder.local_temp(el_ptr_ty.clone());
    builder.cast(data.clone(), data_mem, el_ptr_ty.clone());

    // iteration counter for initializing elements
    let counter = builder.local_temp(Type::I32);
    builder.mov(counter.clone(), Value::LiteralI32(0));

    // loop break flag we use in a couple of places later
    let done = builder.local_temp(Type::Bool);

    // copy elements from copied array
    builder.scope(|builder| {
        let copy_count = builder.local_temp(Type::I32);
        let copy_count_ok = builder.local_temp(Type::Bool);

        // copy_count := src_len
        builder.mov(copy_count.clone(), src_len.clone());

        // if there are more elements in the source than we want, copy `len` elements instead
        // if not (len > copy_count) then copy_count := len
        let copy_count_ok_label = builder.alloc_label();

        builder.gt(copy_count_ok.clone(), len_arg, copy_count.clone());
        builder.jmp_if(copy_count_ok_label, copy_count_ok);
        builder.mov(copy_count.clone(), len_arg);
        builder.label(copy_count_ok_label);

        // for `copy_count` iterations, copy the value at copy_src[counter] to copy_dst[counter]
        let copy_loop_label = builder.alloc_label();
        let copy_break_label = builder.alloc_label();

        // done := counter = copy_count
        // if done then break
        builder.eq(done.clone(), counter.clone(), copy_count.clone());
        builder.jmp_if(copy_break_label, done.clone());

        builder.label(copy_loop_label);
        builder.scope(|builder| {
            let copy_dst = builder.local_temp(el_ptr_ty.clone());
            let copy_src = builder.local_temp(el_ptr_ty.clone());

            // copy_dst := data + counter
            builder.add(copy_dst.clone(), data.clone(), counter.clone());

            // copy_src := src_arr->ptr + counter
            builder.field_val(copy_src.clone(), src_arr.clone(), array_ref_ty.clone(), DYNARRAY_PTR_FIELD, el_ptr_ty.clone());
            builder.add(copy_src.clone(), copy_src.clone(), counter.clone());

            // copy_dst^ := copy_src^
            builder.mov(copy_dst.clone().to_deref(), copy_src.to_deref());

            builder.retain(copy_dst, elem_ty);
        });

        // counter += 1
        builder.add(counter.clone(), counter.clone(), Value::LiteralI32(1));

        builder.jmp(copy_loop_label);
        builder.label(copy_break_label);
    });

    // while counter < len, default init next element
    let init_break_label = builder.alloc_label();
    let init_loop_label = builder.alloc_label();

    builder.label(init_loop_label);

    // done := counter = len
    // if done then break
    builder.eq(done.clone(), counter.clone(), len_arg);
    builder.jmp_if(init_break_label, done);

    builder.scope(|builder| {
        // data[counter] := default_val_ptr^
        let el_ptr = builder.local_temp(el_ptr_ty.clone());
        builder.add(el_ptr.clone(), data.clone(), counter.clone());
        builder.mov(el_ptr.to_deref(), default_el_ptr.clone().to_deref());
    });

    // counter += 1
    builder.add(counter.clone(), counter.clone(), Value::LiteralI32(1));
    builder.jmp(init_loop_label);

    builder.label(init_break_label);

    builder.set_field(arr.clone(), array_ref_ty.clone(), DYNARRAY_LEN_FIELD, Type::I32, len_arg);
    builder.set_field(arr, array_ref_ty, DYNARRAY_PTR_FIELD, el_ptr_ty, data);
}

fn gen_dyn_array_length_func(builder: &mut Builder, struct_id: TypeDefID) {
    let array_ref_ty = Type::RcPointer(VirtualTypeID::Class(struct_id));

    // bind and retain params
    builder.bind_return(Type::I32);
    builder.bind_param(LocalID(1), Type::any(), "arr_ptr", false);
    builder.retain(Ref::Local(LocalID(1)), &Type::any());

    // cast pointer down to this array type
    let arr = builder.local_temp(array_ref_ty.clone());
    builder.cast(arr.clone(), Ref::Local(LocalID(1)), array_ref_ty.clone());

    // evaluate length field into return ref
    builder.field_val(RETURN_REF, arr, array_ref_ty, DYNARRAY_LEN_FIELD, Type::I32);
}

fn gen_dyn_array_rc_boilerplate(module: &mut Module, elem_ty: &Type, struct_id: TypeDefID) {
    let array_ref_ty = Type::RcPointer(VirtualTypeID::Class(struct_id));
    let array_struct_ty = Type::Struct(struct_id);

    let rc_boilerplate = module
        .metadata
        .get_runtime_type(&array_struct_ty)
        .expect("rtti function ids for dynarray inner struct must exist");

    let mut releaser_builder = Builder::new(module);

    // %0 is the self arg, the pointer to the inner struct
    releaser_builder.bind_param(LocalID(0), array_struct_ty.clone().ptr(), "self", true);
    let self_arg = Ref::Local(LocalID(0)).to_deref();

    // pointer to the length field of the dynarray object
    let len_field_ptr = releaser_builder.local_temp(Type::I32.ptr());

    // pointer to the pointer field of the dynarray object
    let arr_field_ptr = releaser_builder.local_temp(elem_ty.clone().ptr().ptr());

    // u8 pointer type field to cast the array memory into to call FreeMem
    let arr_mem_ptr = releaser_builder.local_temp(Type::U8.ptr());

    // iteration vars
    let counter = releaser_builder.local_temp(Type::I32);
    let at_end = releaser_builder.local_temp(Type::Bool);
    let el_ptr = releaser_builder.local_temp(elem_ty.clone().ptr());

    let zero_elements = releaser_builder.local_temp(Type::Bool);

    // jump to loop end if counter == array len
    let start_loop_label = releaser_builder.alloc_label();
    let end_loop_label = releaser_builder.alloc_label();

    let after_free = releaser_builder.alloc_label();

    releaser_builder.field(len_field_ptr.clone(), self_arg.clone(), array_struct_ty.clone(), DYNARRAY_LEN_FIELD);
    releaser_builder.field(arr_field_ptr.clone(), self_arg, array_struct_ty.clone(), DYNARRAY_PTR_FIELD);

    // release every element
    releaser_builder.mov(counter.clone(), Value::LiteralI32(0));

    releaser_builder.label(start_loop_label);

    // at_end := counter eq array.length

    releaser_builder.eq(
        at_end.clone(),
        counter.clone(),
        len_field_ptr.clone().to_deref(),
    );

    // if at_end then break
    releaser_builder.jmp_if(end_loop_label, at_end);

    // release arr[counter]
    releaser_builder.add(el_ptr.clone(), arr_field_ptr.clone().to_deref(), counter.clone());
    releaser_builder.release(el_ptr.to_deref(), &elem_ty);

    // counter := counter + 1
    releaser_builder.add(counter.clone(), counter, Value::LiteralI32(1));

    releaser_builder.jmp(start_loop_label);
    releaser_builder.label(end_loop_label);

    // free the dynamic-allocated buffer - if len > 0
    releaser_builder.eq(zero_elements.clone(), len_field_ptr.clone().to_deref(), Value::LiteralI32(0));
    releaser_builder.jmp_if(after_free, zero_elements);

    releaser_builder.cast(arr_mem_ptr.clone(), arr_field_ptr.clone().to_deref(), Type::U8.ptr());
    releaser_builder.free_mem(arr_mem_ptr);

    releaser_builder.append(Instruction::Label(after_free));

    releaser_builder.mov(len_field_ptr.to_deref(), Value::LiteralI32(0));
    releaser_builder.mov(arr_field_ptr.to_deref(), Value::LiteralNull);

    let releaser_body = releaser_builder.finish();

    let array_ref_ty_name = module.metadata.pretty_ty_name(&array_ref_ty).into_owned();

    module.insert_func(
        rc_boilerplate.release,
        Function::Local(FunctionDef {
            debug_name: format!("<generated dynarray releaser for {}>", array_ref_ty_name),
            return_ty: Type::Nothing,
            params: vec![array_struct_ty.clone().ptr()],
            body: releaser_body,
            src_span: module.module_span().clone(),
        }),
    );

    // no custom retain behaviour (dynarrays can't be retained!)
    module.insert_func(
        rc_boilerplate.retain,
        Function::Local(FunctionDef {
            debug_name: format!("<generated empty retainer for {}>", array_ref_ty_name),
            return_ty: Type::Nothing,
            params: vec![array_struct_ty.clone().ptr()],
            body: Vec::new(),
            src_span: module.module_span().clone()
        }),
    );
}

// class types must generate cleanup code for their inner struct which isn't
// explicitly called in IR but must be called dynamically by the target to
// clean up the inner structs of class RC cells.
// for example, a class instance maybe be stored behind an `Any` reference,
// at which point rc instructions must discover the actual class type
// dynamically from the rc cell's class pointer/class ID
fn gen_class_rc_boilerplate(module: &mut Module, class_ty: &Type) {
    let resource_struct = class_ty
        .rc_resource_class_id()
        .and_then(|class_id| class_id.as_class())
        .expect("resource class of translated class type was not a struct");

    let mut builder = Builder::new(module);
    builder.gen_rc_boilerplate(&Type::Struct(resource_struct));
}

pub fn translate(module: &pas_ty::Module, opts: IROptions) -> Module {
    let metadata = Metadata::new();
    let mut ir_module = Module::new((*module.root_ctx).clone(), metadata, opts);

    let builtin_disposable = pas_ty::builtin_disposable_iface();

    // make sure compiler builtin types are defined e.g. dynamic array types add implementations
    // to Disposable so need that interface to be defined
    let disposable_iface = {
        let mut builder = Builder::new(&mut ir_module);
        let disposable_iface = builder.translate_iface(&builtin_disposable);
        builder.finish();

        disposable_iface
    };

    ir_module.metadata.define_iface(disposable_iface);

    // if String is defined it needs to be defined in the metadata even if it isn't used,
    // for the benefit of the stdlib (it's not defined in the type context with --no-stdlib)
    let string_name = pas_ty::builtin_string_name();
    if let Ok(string_class) = module.root_ctx.find_struct_def(&string_name.qualified) {
        let name = {
            let mut builder = Builder::new(&mut ir_module);
            let name = builder.translate_name(&string_name);
            builder.finish();
            name
        };

        ir_module.metadata.reserve_struct(STRING_ID);
        ir_module.metadata.declare_struct(STRING_ID, &name);

        let string_def = {
            let mut builder = Builder::new(&mut ir_module);
            let string_def = builder.translate_class(&string_class);
            builder.finish();
            string_def
        };

        ir_module.metadata.define_struct(STRING_ID, string_def);

        let mut rc_builder = Builder::new(&mut ir_module);
        rc_builder.gen_rc_boilerplate(&Type::Struct(STRING_ID));
        rc_builder.finish();
    }

    for unit in &module.units {
        ir_module.translate_unit(&unit.unit);
    }

    // add static closure init functions at top of init
    let mut static_closures_init = Vec::new();
    for static_closure in ir_module.static_closures() {
        static_closures_init.push(Instruction::Call {
            function: Value::Ref(Ref::Global(GlobalRef::Function(static_closure.init_func))),
            args: Vec::new(),
            out: None,
        });
    }
    static_closures_init.append(&mut ir_module.init);
    ir_module.init = static_closures_init;

    ir_module.gen_iface_impls();
    for (elem_ty, struct_id) in ir_module.metadata.dyn_array_structs().clone() {
        gen_dyn_array_rc_boilerplate(&mut ir_module, &elem_ty, struct_id);
        gen_dyn_array_funcs(&mut ir_module, &elem_ty, struct_id);
    }
    for class_ty in ir_module.class_types().cloned().collect::<Vec<_>>() {
        gen_class_rc_boilerplate(&mut ir_module, &class_ty);
    }
    for closure_id in ir_module.closure_types().collect::<Vec<_>>() {
        let mut builder = Builder::new(&mut ir_module);
        builder.gen_rc_boilerplate(&Type::Struct(closure_id));
    }

    ir_module.metadata.sort_type_defs_by_deps();

    ir_module
}
