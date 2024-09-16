use crate::instruction::Instruction;
use crate::metadata::{InterfaceID, MethodID};
use crate::ty::{FieldID, Type, VirtualTypeID};
use crate::val::{Ref, Value};
use crate::NamePath;
use std::{cell::Cell, fmt};

pub trait InstructionFormatter {
    fn format_instruction<W: fmt::Write>(
        &self,
        instruction: &Instruction,
        f: &mut W,
    ) -> fmt::Result {
        const IX_WIDTH: usize = 8;
        match instruction {
            Instruction::Comment(comment) => write!(f, "{:>width$} {}", "//", comment, width = IX_WIDTH),

            Instruction::DebugPush(span) => {
                write!(f, "{:>width$} {}", "dbgpush", span, width = IX_WIDTH)
            }

            Instruction::DebugPop => {
                write!(f, "{:>width$}", "dbgpop", width = IX_WIDTH)
            }

            Instruction::LocalAlloc(id, ty) => {
                write!(f, "{:>width$} ", "local", width = IX_WIDTH)?;
                self.format_ref(&Ref::Local(*id), f)?;
                write!(f, " of ")?;
                self.format_type(ty, f)
            }
            Instruction::LocalBegin => write!(f, "{:>width$} ", "begin", width = IX_WIDTH),
            Instruction::LocalEnd => write!(f, "{:>width$} ", "end", width = IX_WIDTH),

            Instruction::Move { out, new_val } => {
                write!(f, "{:>width$} ", "mov", width = IX_WIDTH)?;

                self.format_ref(out, f)?;
                write!(f, " := ")?;
                self.format_val(new_val, f)
            }
            Instruction::Add { out, a, b } => {
                write!(f, "{:>width$} ", "add", width = IX_WIDTH)?;

                self.format_ref(out, f)?;
                write!(f, " := ")?;
                self.format_val(a, f)?;
                write!(f, " + ")?;
                self.format_val(b, f)
            }
            Instruction::Sub { out, a, b } => {
                write!(f, "{:>width$} ", "sub", width = IX_WIDTH)?;

                self.format_ref(out, f)?;
                write!(f, " := ")?;
                self.format_val(a, f)?;
                write!(f, " - ")?;
                self.format_val(b, f)
            }
            Instruction::Mul { out, a, b } => {
                write!(f, "{:>width$} ", "mul", width = IX_WIDTH)?;

                self.format_ref(out, f)?;
                write!(f, " := ")?;
                self.format_val(a, f)?;
                write!(f, " * ")?;
                self.format_val(b, f)
            }
            Instruction::Div { out, a, b } => {
                write!(f, "{:>width$} ", "idiv", width = IX_WIDTH)?;

                self.format_ref(out, f)?;
                write!(f, " := ")?;
                self.format_val(a, f)?;
                write!(f, " div ")?;
                self.format_val(b, f)
            }
            Instruction::Mod { out, a, b } => {
                write!(f, "{:>width$} ", "mod", width = IX_WIDTH)?;

                self.format_ref(out, f)?;
                write!(f, " := ")?;
                self.format_val(a, f)?;
                write!(f, " mod ")?;
                self.format_val(b, f)
            }
            Instruction::Shl { out, a, b } => {
                write!(f, "{:>width$} ", "shl", width = IX_WIDTH)?;

                self.format_ref(out, f)?;
                write!(f, " := ")?;
                self.format_val(a, f)?;
                write!(f, " shl ")?;
                self.format_val(b, f)
            }
            Instruction::Shr { out, a, b } => {
                write!(f, "{:>width$} ", "shr", width = IX_WIDTH)?;

                self.format_ref(out, f)?;
                write!(f, " := ")?;
                self.format_val(a, f)?;
                write!(f, " shr ")?;
                self.format_val(b, f)
            }

            Instruction::BitAnd { out, a, b } => {
                write!(f, "{:>width$} ", "bitand", width = IX_WIDTH)?;

                self.format_ref(out, f)?;
                write!(f, " := ")?;
                self.format_val(a, f)?;
                write!(f, " & ")?;
                self.format_val(b, f)
            }
            Instruction::BitOr { out, a, b } => {
                write!(f, "{:>width$} ", "bitor", width = IX_WIDTH)?;

                self.format_ref(out, f)?;
                write!(f, " := ")?;
                self.format_val(a, f)?;
                write!(f, " | ")?;
                self.format_val(b, f)
            }
            Instruction::BitXor { out, a, b } => {
                write!(f, "{:>width$} ", "bitxor", width = IX_WIDTH)?;

                self.format_ref(out, f)?;
                write!(f, " := ")?;
                self.format_val(a, f)?;
                write!(f, " ^ ")?;
                self.format_val(b, f)
            }
            Instruction::BitNot { out, a } => {
                write!(f, "{:>width$} ", "bitnot", width = IX_WIDTH)?;

                self.format_ref(out, f)?;
                write!(f, " := ~")?;
                self.format_val(a, f)
            }

            Instruction::Eq { out, a, b } => {
                write!(f, "{:>width$} ", "eq", width = IX_WIDTH)?;

                self.format_ref(out, f)?;
                write!(f, " := ")?;
                self.format_val(a, f)?;
                write!(f, " = ")?;
                self.format_val(b, f)
            }
            Instruction::Gt { out, a, b } => {
                write!(f, "{:>width$} ", "gt", width = IX_WIDTH)?;

                self.format_ref(out, f)?;
                write!(f, " := ")?;
                self.format_val(a, f)?;
                write!(f, " > ")?;
                self.format_val(b, f)
            }
            Instruction::Not { out, a } => {
                write!(f, "{:>width$} ", "not", width = IX_WIDTH)?;

                self.format_ref(out, f)?;
                write!(f, " := ~")?;
                self.format_val(a, f)
            }
            Instruction::And { out, a, b } => {
                write!(f, "{:>width$} ", "and", width = IX_WIDTH)?;

                self.format_ref(out, f)?;
                write!(f, " := ")?;
                self.format_val(a, f)?;
                write!(f, " and ")?;
                self.format_val(b, f)
            }
            Instruction::Or { out, a, b } => {
                write!(f, "{:>width$} ", "or", width = IX_WIDTH)?;

                self.format_ref(out, f)?;
                write!(f, " := ")?;
                self.format_val(a, f)?;
                write!(f, " or ")?;
                self.format_val(b, f)
            }

            Instruction::Call {
                out,
                function,
                args,
            } => {
                write!(f, "{:>width$} ", "call", width = IX_WIDTH)?;
                if let Some(out) = out {
                    self.format_ref(out, f)?;
                    write!(f, " := ")?;
                }

                self.format_val(function, f)?;

                write!(f, "(")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    self.format_val(arg, f)?;
                }
                write!(f, ")")
            }

            Instruction::VirtualCall {
                out,
                iface_id,
                method,
                self_arg,
                rest_args,
            } => {
                write!(f, "{:>width$} ", "vcall", width = IX_WIDTH)?;

                if let Some(out) = out {
                    self.format_ref(out, f)?;
                    write!(f, " := ")?;
                }
                write!(f, "(")?;

                self.format_val(self_arg, f)?;
                write!(f, " as ")?;
                self.format_type(&Type::RcPointer(VirtualTypeID::Interface(*iface_id)), f)?;

                write!(f, ").")?;
                self.format_method(*iface_id, *method, f)?;
                write!(f, "(")?;

                for (i, arg) in rest_args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    self.format_val(arg, f)?;
                }
                write!(f, ")")
            }

            Instruction::ClassIs { out, a, class_id } => {
                write!(f, "{:>width$} ", "is", width = IX_WIDTH)?;

                self.format_ref(out, f)?;
                write!(f, " := ")?;
                self.format_val(a, f)?;
                write!(f, " is ")?;
                self.format_type(&Type::RcPointer(*class_id), f)
            }

            Instruction::AddrOf { out, a } => {
                write!(f, "{:>width$} ", "addrof", width = IX_WIDTH)?;

                self.format_ref(out, f)?;
                write!(f, " := @")?;
                self.format_ref(a, f)
            }

            Instruction::Element {
                out,
                a,
                element,
                index,
            } => {
                write!(f, "{:>width$} ", "el", width = IX_WIDTH)?;

                self.format_ref(out, f)?;
                write!(f, " := @(")?;
                self.format_ref(a, f)?;
                write!(f, " as array of ")?;
                self.format_type(element, f)?;
                write!(f, ")[")?;
                self.format_val(index, f)?;
                write!(f, "]")
            }

            Instruction::Field {
                out,
                a,
                of_ty,
                field,
            } => {
                write!(f, "{:>width$} ", "field", width = IX_WIDTH)?;

                self.format_ref(out, f)?;
                write!(f, " := @(")?;
                self.format_ref(a, f)?;
                write!(f, " as ")?;
                self.format_type(of_ty, f)?;
                write!(f, ").")?;
                self.format_field(of_ty, *field, f)
            }

            Instruction::VariantTag { out, a, of_ty } => {
                write!(f, "{:>width$} ", "vartag", width = IX_WIDTH)?;
                self.format_ref(out, f)?;
                write!(f, " := @(")?;
                self.format_ref(a, f)?;
                write!(f, " as ")?;
                self.format_type(of_ty, f)?;
                write!(f, ").tag")
            }

            Instruction::VariantData { out, a, of_ty, tag } => {
                write!(f, "{:>width$} ", "vardata", width = IX_WIDTH)?;
                self.format_ref(out, f)?;
                write!(f, " := @(")?;
                self.format_ref(a, f)?;
                write!(f, " as ")?;
                self.format_type(of_ty, f)?;
                write!(f, ").")?;
                self.format_variant_case(of_ty, *tag, f)
            }

            Instruction::Label(label) => {
                write!(f, "{:>width$} {}", "label", label, width = IX_WIDTH)
            }

            Instruction::Jump { dest } => write!(f, "{:>width$} {}", "jmp", dest, width = IX_WIDTH),

            Instruction::JumpIf { dest, test } => {
                write!(f, "{:>width$} {} if ", "jmpif", dest, width = IX_WIDTH)?;
                self.format_val(test, f)
            }

            Instruction::RcNew { out, struct_id } => {
                write!(f, "{:>width$} ", "rcnew", width = IX_WIDTH)?;
                self.format_type(&Type::Struct(*struct_id), f)?;
                write!(f, " at {}", out)
            }

            Instruction::Release { at } => {
                write!(f, "{:>width$} {}", "release", at, width = IX_WIDTH)
            }

            Instruction::Retain { at } => {
                write!(f, "{:>width$} {}", "retain", at, width = IX_WIDTH)
            }

            Instruction::Raise { val } => {
                write!(f, "{:>width$} {}", "raise", val, width = IX_WIDTH)
            }

            Instruction::Cast { out, ty, a } => {
                write!(f, "{:>width$} {} := {} as ", "cast", out, a, width = IX_WIDTH)?;
                self.format_type(ty, f)
            }
        }
    }

    fn format_type(&self, ty: &Type, f: &mut dyn fmt::Write) -> fmt::Result;
    fn format_val(&self, val: &Value, f: &mut dyn fmt::Write) -> fmt::Result;
    fn format_ref(&self, r: &Ref, f: &mut dyn fmt::Write) -> fmt::Result;
    fn format_field(&self, of_ty: &Type, field: FieldID, f: &mut dyn fmt::Write) -> fmt::Result;
    fn format_method(
        &self,
        iface: InterfaceID,
        method: MethodID,
        f: &mut dyn fmt::Write,
    ) -> fmt::Result;
    fn format_variant_case(&self, of_ty: &Type, tag: usize, f: &mut dyn fmt::Write) -> fmt::Result;

    fn format_name(&self, name: &NamePath, f: &mut dyn fmt::Write) -> fmt::Result {
        write!(f, "{}", name.path.join("::"))?;

        if let Some(name_type_args) = name.type_args.as_ref() {
            write!(f, "<")?;
            for (i, arg) in name_type_args.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }

                self.format_type(arg, f)?;
            }
            write!(f, ">")?;
        }
        Ok(())
    }
}

pub struct RawInstructionFormatter;

impl InstructionFormatter for RawInstructionFormatter {
    fn format_type(&self, ty: &Type, f: &mut dyn fmt::Write) -> fmt::Result {
        write!(f, "{}", ty)
    }

    fn format_val(&self, val: &Value, f: &mut dyn fmt::Write) -> fmt::Result {
        write!(f, "{}", val)
    }

    fn format_ref(&self, r: &Ref, f: &mut dyn fmt::Write) -> fmt::Result {
        write!(f, "{}", r)
    }

    fn format_field(&self, _of_ty: &Type, field: FieldID, f: &mut dyn fmt::Write) -> fmt::Result {
        write!(f, "{}", field)
    }

    fn format_method(
        &self,
        _iface_id: InterfaceID,
        method: MethodID,
        f: &mut dyn fmt::Write,
    ) -> fmt::Result {
        write!(f, "<method {}>", method.0)
    }

    fn format_variant_case(
        &self,
        _of_ty: &Type,
        tag: usize,
        f: &mut dyn fmt::Write,
    ) -> fmt::Result {
        write!(f, "data_{}", tag)
    }
}

pub struct StatefulIndentedFormatter<'f, F: InstructionFormatter> {
    wrapped: &'f F,
    tabs: Cell<usize>,
    tab_width: usize,
}

impl<'f, F: InstructionFormatter> StatefulIndentedFormatter<'f, F> {
    pub fn new(wrapped: &'f F, tab_width: usize) -> Self {
        Self {
            wrapped,
            tabs: Cell::new(0),
            tab_width,
        }
    }
}

impl<'f, F: InstructionFormatter> InstructionFormatter for StatefulIndentedFormatter<'f, F> {
    fn format_instruction<W: fmt::Write>(
        &self,
        instruction: &Instruction,
        f: &mut W,
    ) -> fmt::Result {
        if let Instruction::LocalEnd = instruction {
            self.tabs.set(self.tabs.get() - 1);
        }

        let tabs = match instruction {
            Instruction::Label(..) => 0,
            _ => self.tabs.get(),
        };

        for _ in 0..tabs * self.tab_width {
            f.write_char(' ')?;
        }

        if let Instruction::LocalBegin = instruction {
            self.tabs.set(self.tabs.get() + 1);
        }

        self.wrapped.format_instruction(instruction, f)
    }

    fn format_type(&self, ty: &Type, f: &mut dyn fmt::Write) -> fmt::Result {
        self.wrapped.format_type(ty, f)
    }

    fn format_val(&self, val: &Value, f: &mut dyn fmt::Write) -> fmt::Result {
        self.wrapped.format_val(val, f)
    }

    fn format_ref(&self, r: &Ref, f: &mut dyn fmt::Write) -> fmt::Result {
        self.wrapped.format_ref(r, f)
    }

    fn format_field(&self, of_ty: &Type, field: FieldID, f: &mut dyn fmt::Write) -> fmt::Result {
        self.wrapped.format_field(of_ty, field, f)
    }

    fn format_method(
        &self,
        iface: InterfaceID,
        method: MethodID,
        f: &mut dyn fmt::Write,
    ) -> fmt::Result {
        self.wrapped.format_method(iface, method, f)
    }

    fn format_variant_case(&self, of_ty: &Type, tag: usize, f: &mut dyn fmt::Write) -> fmt::Result {
        self.wrapped.format_variant_case(of_ty, tag, f)
    }
}
