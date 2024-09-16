use std::fmt;
use serde::{Deserialize, Serialize};
use common::span::Span;
use crate::formatter::{InstructionFormatter, RawInstructionFormatter};
use crate::metadata::{InterfaceID, MethodID, TypeDefID};
use crate::ty::{FieldID, Type, VirtualTypeID};
use crate::val::{LocalID, Ref, Value};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Instruction {
    Comment(String),
    DebugPush(Span),
    DebugPop,

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
    Div {
        out: Ref,
        a: Value,
        b: Value,
    },
    Mod {
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
    BitAnd {
        out: Ref,
        a: Value,
        b: Value,
    },
    BitOr {
        out: Ref,
        a: Value,
        b: Value,
    },
    BitXor {
        out: Ref,
        a: Value,
        b: Value,
    },
    BitNot {
        out: Ref,
        a: Value,
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
        class_id: VirtualTypeID,
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
        struct_id: TypeDefID,
    },

    Release {
        at: Ref,
    },
    Retain {
        at: Ref,
    },

    Raise {
        val: Ref,
    },

    Cast {
        out: Ref,
        ty: Type,
        a: Value,
    }
}

impl Instruction {
    // can this instruction be discarded? usually this means its parameters make it a noop,
    // e.g. mov or arithmetic into to a discard ref.
    //
    // intentional noop instructions like comments and debug info are not considered discardable.
    pub fn should_discard(&self) -> bool {
        match self {
            // never discard
            | Instruction::Comment(..)
            | Instruction::DebugPush(..)
            | Instruction::DebugPop
            | Instruction::LocalBegin
            | Instruction::LocalEnd
            | Instruction::Label(..)
            | Instruction::Jump { .. }
            | Instruction::JumpIf { .. }
            | Instruction::Raise { .. }
            | Instruction::VirtualCall { .. }
            | Instruction::Call { .. }
            | Instruction::LocalAlloc(..) => false,

            // instructions that mutate state
            // discard if they operate on a discard ref
            | Instruction::Release { at }
            | Instruction::Retain { at } => *at == Ref::Discard,

            // mov: discard if either the origin or destination refs are discards
            | Instruction::Move { out: Ref::Discard, .. }
            | Instruction::Move { new_val: Value::Ref(Ref::Discard), .. } => true,
            | Instruction::Move { .. } => false,

            // operator instructions
            // discard if they output into a discard ref
            | Instruction::Add { out, .. }
            | Instruction::Sub { out, .. }
            | Instruction::Mul { out, .. }
            | Instruction::Div { out, .. }
            | Instruction::Mod { out, .. }
            | Instruction::Shl { out, .. }
            | Instruction::Shr { out, .. }
            | Instruction::Eq { out, .. }
            | Instruction::Gt { out, .. }
            | Instruction::Not { out, .. }
            | Instruction::And { out, .. }
            | Instruction::Or { out, .. }
            | Instruction::BitOr { out, .. }
            | Instruction::BitAnd { out, .. }
            | Instruction::BitXor { out, .. }
            | Instruction::BitNot { out, .. }
            | Instruction::AddrOf { out, .. }
            | Instruction::Element { out, .. }
            | Instruction::VariantTag { out, .. }
            | Instruction::VariantData { out, .. }
            | Instruction::Field { out, .. }
            | Instruction::ClassIs { out, .. }
            | Instruction::RcNew { out, .. }
            | Instruction::Cast { out, .. } => *out == Ref::Discard,
        }
    }
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

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct Label(pub usize);

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:", self.0)
    }
}
