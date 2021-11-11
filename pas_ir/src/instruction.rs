use std::fmt;
use crate::{Ref, Value, Type, LocalID, InterfaceID, MethodID, FieldID, StructID, ClassID, RawInstructionFormatter, InstructionFormatter};

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    Comment(String),

    LocalAlloc(LocalID, Type),
    LocalBegin,
    LocalEnd,

    Move {
        out: Ref,
        new_val: Value,
    },
    Add {
        out: Ref,
        a: Value,
        b: Value,
    },
    Sub {
        out: Ref,
        a: Value,
        b: Value,
    },
    Mul {
        out: Ref,
        a: Value,
        b: Value,
    },
    IDiv {
        out: Ref,
        a: Value,
        b: Value,
    },
    Shl {
        out: Ref,
        a: Value,
        b: Value,
    },
    Shr {
        out: Ref,
        a: Value,
        b: Value,
    },

    Eq {
        out: Ref,
        a: Value,
        b: Value,
    },
    Gt {
        out: Ref,
        a: Value,
        b: Value,
    },
    Not {
        out: Ref,
        a: Value,
    },
    And {
        out: Ref,
        a: Value,
        b: Value,
    },
    Or {
        out: Ref,
        a: Value,
        b: Value,
    },

    /// Stores a pointer to `a` into `out`
    AddrOf {
        out: Ref,
        a: Ref,
    },

    /// Stores the address of an array element from array at `a` into `out`
    Element {
        out: Ref,
        a: Ref,
        index: Value,
        element: Type,
    },

    /// stores a pointer to the tag of a variant at `a` into `out`
    VariantTag {
        out: Ref,
        a: Ref,
        of_ty: Type,
    },
    /// stores a pointer to the data for a variant case of index `tag` at `a` into `out`
    VariantData {
        out: Ref,
        a: Ref,
        tag: usize,
        of_ty: Type,
    },

    /// Stores the address of an object field from object of type `of_ty` at location `a` into `out`.
    /// `of_ty` must match the type of the value stored at `a` and must also be a structured type
    /// i.e. one that has fields (struct or RC-pointer to struct).
    Field {
        out: Ref,
        a: Ref,
        of_ty: Type,
        field: FieldID,
    },

    Call {
        out: Option<Ref>,
        function: Value,
        args: Vec<Value>,
    },
    VirtualCall {
        out: Option<Ref>,
        iface_id: InterfaceID,
        method: MethodID,
        self_arg: Value,
        rest_args: Vec<Value>,
    },
    ClassIs {
        out: Ref,
        a: Value,
        class_id: ClassID,
    },

    Label(Label),
    Jump {
        dest: Label,
    },
    JumpIf {
        dest: Label,
        test: Value,
    },

    RcNew {
        out: Ref,
        struct_id: StructID,
    },

    Release {
        at: Ref,
    },
    Retain {
        at: Ref,
    },

    DynAlloc {
        out: Ref,
        element_ty: Type,
        len: Value,
    },
    DynFree {
        at: Ref,
    },

    Raise {
        val: Ref,
    },

    SizeOf {
        out: Ref,
        ty: Type,
    },
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut buf = String::new();
        RawInstructionFormatter
            .format_instruction(self, &mut buf)
            .map_err(|_| fmt::Error)?;

        f.write_str(&buf)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Label(pub usize);

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:", self.0)
    }
}

pub fn jmp_exists(instructions: &[Instruction], to_label: Label) -> bool {
    instructions.iter().any(|i| match i {
        Instruction::Jump { dest } | Instruction::JumpIf { dest, .. } => *dest == to_label,
        _ => false,
    })
}

