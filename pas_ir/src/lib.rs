use std::fmt;
use pas_common::span::Span;

pub use self::{formatter::*, instruction::*, metadata::ty::Type, module::*, val::*};
use crate::ty::{ClassID, FieldID, TypeDef};
use crate::{builder::Builder, expr::*, metadata::*, stmt::*};
use pas_syn::IdentPath;
use pas_typecheck as pas_ty;
use pas_typecheck::builtin_span;

mod builder;
mod dep_sort;
mod expr;
mod formatter;
mod instruction;
pub mod metadata;
mod module;
mod stmt;
mod val;
pub mod prelude {
    pub use crate::{
        instruction::*, metadata::ty::*, metadata::*, GlobalRef, Instruction, Label, Ref, Value,
        RETURN_REF,
    };
}

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

#[derive(Clone, Debug)]
pub struct ExternalFunctionRef {
    pub symbol: String,
    pub src: String,

    pub return_ty: Type,
    pub params: Vec<Type>,

    pub src_span: Span,
}

#[derive(Clone, Debug)]
pub struct FunctionDef {
    pub debug_name: String,

    pub body: Vec<Instruction>,
    pub return_ty: Type,
    pub params: Vec<Type>,

    pub src_span: Span,
}

#[derive(Clone, Debug)]
pub enum Function {
    External(ExternalFunctionRef),
    Local(FunctionDef),
}

impl Function {
    pub fn debug_name(&self) -> &str {
        match self {
            Function::External(ExternalFunctionRef { symbol, .. }) => symbol.as_str(),
            Function::Local(FunctionDef { debug_name, .. }) => debug_name.as_str(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct FunctionInstance {
    pub id: FunctionID,
    pub sig: pas_ty::FunctionSig,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
enum FunctionDeclKey {
    Function {
        name: IdentPath,
    },
    Method {
        iface: IdentPath,
        self_ty: pas_ty::Type,
        method: pas_syn::Ident,
    },
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub(crate) struct FunctionCacheKey {
    decl_key: FunctionDeclKey,
    type_args: Option<pas_ty::TypeList>,
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

fn gen_dyn_array_rc_boilerplate(module: &mut Module, elem_ty: &Type, struct_id: StructID) {
    let array_class = ClassID::Class(struct_id);
    let array_ref_ty = Type::RcPointer(Some(array_class));

    // the thing we're cleaning up is the internal struct, not the rc object itself,
    // so the parameter type of the releaser will be a pointer to that struct
    let array_struct_ty = Type::Struct(struct_id);

    let rc_boilerplate = module
        .metadata
        .find_rc_boilerplate(&array_struct_ty)
        .expect("rc boilerplate function ids for dynarray inner struct must exist");

    let mut releaser_builder = Builder::new(module, builtin_span());

    // %0 is the self arg, the pointer to the inner struct
    releaser_builder.bind_param(LocalID(0), array_struct_ty.clone().ptr(), "self", true);
    let self_arg = Ref::Local(LocalID(0)).to_deref();

    let len_field_ptr = releaser_builder.local_temp(Type::I32.ptr());
    releaser_builder.append(Instruction::Field {
        out: len_field_ptr.clone(),
        of_ty: array_struct_ty.clone(),
        field: DYNARRAY_LEN_FIELD,
        a: self_arg.clone(),
    });

    let arr_field_ptr = releaser_builder.local_temp(elem_ty.clone().ptr().ptr());
    releaser_builder.append(Instruction::Field {
        out: arr_field_ptr.clone(),
        of_ty: array_struct_ty.clone(),
        field: DYNARRAY_PTR_FIELD,
        a: self_arg,
    });

    // release every element
    let counter = releaser_builder.local_temp(Type::I32);
    releaser_builder.mov(counter.clone(), Value::LiteralI32(0));

    // jump to loop end if counter == array len
    let start_loop_label = releaser_builder.alloc_label();
    let end_loop_label = releaser_builder.alloc_label();

    releaser_builder.append(Instruction::Label(start_loop_label));

    // at_end := counter eq array.length
    let at_end = releaser_builder.local_temp(Type::Bool);
    releaser_builder.eq(
        at_end.clone(),
        counter.clone(),
        len_field_ptr.clone().to_deref(),
    );

    // if at_end then break
    releaser_builder.append(Instruction::JumpIf {
        dest: end_loop_label,
        test: Value::Ref(at_end),
    });

    // release arr[counter]
    let el_ptr = releaser_builder.local_temp(elem_ty.clone().ptr());
    releaser_builder.append(Instruction::Add {
        out: el_ptr.clone(),
        a: Value::Ref(arr_field_ptr.clone().to_deref()),
        b: Value::Ref(counter.clone()),
    });
    releaser_builder.release(el_ptr.to_deref(), &elem_ty);

    // counter := counter + 1
    releaser_builder.append(Instruction::Add {
        out: counter.clone(),
        a: Value::Ref(counter),
        b: Value::LiteralI32(1),
    });

    releaser_builder.append(Instruction::Jump {
        dest: start_loop_label,
    });
    releaser_builder.append(Instruction::Label(end_loop_label));

    // free the dynamic-allocated buffer - if len > 0
    let after_free = releaser_builder.alloc_label();

    let zero_elements = releaser_builder.local_temp(Type::Bool);
    releaser_builder.append(Instruction::Eq {
        a: Value::Ref(len_field_ptr.clone().to_deref()),
        b: Value::LiteralI32(0),
        out: zero_elements.clone(),
    });
    releaser_builder.append(Instruction::JumpIf {
        dest: after_free,
        test: Value::Ref(zero_elements),
    });

    releaser_builder.append(Instruction::DynFree {
        at: arr_field_ptr.clone().to_deref(),
    });

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
            src_span: builtin_span(),
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
            src_span: builtin_span(),
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

    let mut builder = Builder::new(module, builtin_span());
    builder.gen_rc_boilerplate(&Type::Struct(resource_struct));
}

pub fn translate(module: &pas_ty::Module, opts: IROptions) -> Module {
    let metadata = Metadata::new();
    let mut ir_module = Module::new(module.root_ctx.clone(), metadata, opts);

    let builtin_disposable = pas_ty::builtin_disposable_iface();

    // make sure compiler builtin types are defined e.g. dynamic array types add implementations
    // to Disposable so need that interface to be defined
    let disposable_iface = {
        let mut builder = Builder::new(&mut ir_module, builtin_disposable.span.clone());
        let disposable_iface = builder.translate_iface(&builtin_disposable);
        builder.finish();

        disposable_iface
    };

    ir_module.metadata.define_iface(disposable_iface);

    // if String is defined it needs to be defined in the metadata even if it isn't used,
    // for the benefit of the stdlib (it's not defined in the type context with --no-stdlib)
    let string_name = pas_ty::builtin_string_name();
    if let Ok(string_class) = module.root_ctx.find_class_def(&string_name.qualified) {
        let name = {
            let mut builder = Builder::new(&mut ir_module, string_class.span.clone());
            let name = builder.translate_name(&string_name);
            builder.finish();
            name
        };

        ir_module.metadata.reserve_struct(STRING_ID);
        ir_module.metadata.declare_struct(STRING_ID, &name);

        let string_def = {
            let mut builder = Builder::new(&mut ir_module, string_class.span.clone());
            let string_def = builder.translate_class(&string_class);
            builder.finish();
            string_def
        };

        ir_module.metadata.define_struct(STRING_ID, string_def);

        let mut rc_builder = Builder::new(&mut ir_module, string_class.span.clone());
        rc_builder.gen_rc_boilerplate(&Type::Struct(STRING_ID));
        rc_builder.finish();
    }

    for unit in &module.units {
        ir_module.translate_unit(&unit.unit);
    }

    ir_module.gen_iface_impls();
    for (elem_ty, struct_id) in ir_module.metadata.dyn_array_structs().clone() {
        gen_dyn_array_rc_boilerplate(&mut ir_module, &elem_ty, struct_id);
    }
    for class_ty in ir_module.class_types().cloned().collect::<Vec<_>>() {
        gen_class_rc_boilerplate(&mut ir_module, &class_ty);
    }

    ir_module.metadata.sort_type_defs_by_deps();

    ir_module
}
