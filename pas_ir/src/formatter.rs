use std::{cell::Cell, fmt};

use crate::{metadata::*, GlobalRef, Instruction, Ref, Value};

pub trait InstructionFormatter {
    fn format_instruction<W: fmt::Write>(
        &self,
        instruction: &Instruction,
        f: &mut W,
    ) -> fmt::Result {
        const IX_WIDTH: usize = 8;
        match instruction {
            Instruction::Comment(comment) => write!(f, "// {}", comment),

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
                self.format_type(&Type::RcPointer(Some(ClassID::Interface(*iface_id))), f)?;
                write!(f, ").{}(", method.0)?;
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
                self.format_type(&Type::RcPointer(Some(*class_id)), f)
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
                write!(f, "{:>width$} ", "fld", width = IX_WIDTH)?;

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
                write!(f, " at {}^", out)
            }

            Instruction::Release { at } => {
                write!(f, "{:>width$} {}", "release", at, width = IX_WIDTH)
            }

            Instruction::Retain { at } => {
                write!(f, "{:>width$} {}", "retain", at, width = IX_WIDTH)
            }

            Instruction::DynAlloc {
                out,
                element_ty,
                len,
            } => write!(
                f,
                "{:>width$} {} := {}[{}]",
                "dynalloc",
                out,
                element_ty,
                len,
                width = IX_WIDTH
            ),

            Instruction::DynFree { at } => {
                write!(f, "{:>width$} {}", "dynfree", at, width = IX_WIDTH)
            }
        }
    }

    fn format_type(&self, ty: &Type, f: &mut fmt::Write) -> fmt::Result;
    fn format_val(&self, val: &Value, f: &mut fmt::Write) -> fmt::Result;
    fn format_ref(&self, r: &Ref, f: &mut fmt::Write) -> fmt::Result;
    fn format_field(&self, of_ty: &Type, field: FieldID, f: &mut fmt::Write) -> fmt::Result;
    fn format_variant_case(&self, of_ty: &Type, tag: usize, f: &mut fmt::Write) -> fmt::Result;

    fn format_name(&self, name: &NamePath, f: &mut fmt::Write) -> fmt::Result {
        write!(f, "{}", name.path.join("::"))?;

        if !name.type_args.is_empty() {
            write!(f, "<")?;
            for (i, arg) in name.type_args.iter().enumerate() {
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
    fn format_type(&self, ty: &Type, f: &mut fmt::Write) -> fmt::Result {
        write!(f, "{}", ty)
    }

    fn format_val(&self, val: &Value, f: &mut fmt::Write) -> fmt::Result {
        write!(f, "{}", val)
    }

    fn format_ref(&self, r: &Ref, f: &mut fmt::Write) -> fmt::Result {
        write!(f, "{}", r)
    }

    fn format_field(&self, _of_ty: &Type, field: FieldID, f: &mut fmt::Write) -> fmt::Result {
        write!(f, "{}", field)
    }

    fn format_variant_case(&self, _of_ty: &Type, tag: usize, f: &mut fmt::Write) -> fmt::Result {
        write!(f, "data_{}", tag)
    }
}

impl InstructionFormatter for Metadata {
    fn format_type(&self, ty: &Type, f: &mut fmt::Write) -> fmt::Result {
        write!(f, "{}", self.pretty_ty_name(ty))
    }

    fn format_val(&self, val: &Value, f: &mut fmt::Write) -> fmt::Result {
        match val {
            Value::Ref(r) => self.format_ref(r, f),
            _ => RawInstructionFormatter.format_val(val, f),
        }
    }

    fn format_ref(&self, r: &Ref, f: &mut fmt::Write) -> fmt::Result {
        match r {
            Ref::Global(GlobalRef::StringLiteral(string_id)) => match self.get_string(*string_id) {
                Some(string_lit) => write!(f, "'{}'", string_lit),
                None => write!(f, "{}", r),
            },

            Ref::Global(GlobalRef::Function(id)) => {
                let func_name = self.get_function(*id).and_then(|f| f.global_name.as_ref());

                match func_name {
                    Some(name) => write!(f, "{}", name),

                    None => {
                        let find_iface_impl = self.ifaces().iter().find_map(|(_id, iface)| {
                            iface.impls.iter().find_map(|(impl_ty, iface_impl)| {
                                let method_id = iface_impl.methods.iter().find_map(
                                    |(method_id, func_id)| {
                                        if *func_id == *id {
                                            Some(method_id)
                                        } else {
                                            None
                                        }
                                    },
                                )?;

                                let method = iface.get_method(*method_id).unwrap();

                                Some((&iface.name, impl_ty, &method.name))
                            })
                        });

                        match find_iface_impl {
                            Some((iface_name, impl_ty, method_name)) => {
                                write!(f, "{}.{} impl for ", iface_name, method_name)?;
                                self.format_type(impl_ty, f)
                            }

                            None => write!(f, "{}", r),
                        }
                    }
                }
            }

            _ => RawInstructionFormatter.format_ref(r, f),
        }
    }

    fn format_field(&self, of_ty: &Type, field: FieldID, f: &mut fmt::Write) -> fmt::Result {
        let field_name = of_ty
            .as_struct()
            .or_else(|| match of_ty.rc_resource_class_id()? {
                ClassID::Class(struct_id) => Some(struct_id),
                _ => None,
            })
            .and_then(|struct_id| self.get_struct_def(struct_id))
            .and_then(|struct_def| struct_def.fields.get(&field))
            .map(|field| &field.name);

        match field_name {
            Some(name) => write!(f, "{}", name),
            _ => RawInstructionFormatter.format_field(of_ty, field, f),
        }
    }

    fn format_variant_case(&self, of_ty: &Type, tag: usize, f: &mut fmt::Write) -> fmt::Result {
        let case_name = match of_ty {
            Type::Variant(id) => self
                .get_variant_def(*id)
                .and_then(|variant| variant.cases.get(tag))
                .map(|case| &case.name),
            _ => None,
        };

        match case_name {
            Some(name) => write!(f, "{}", name),
            _ => RawInstructionFormatter.format_variant_case(of_ty, tag, f),
        }
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

        for _ in 0..self.tabs.get() * self.tab_width {
            f.write_char(' ')?;
        }

        if let Instruction::LocalBegin = instruction {
            self.tabs.set(self.tabs.get() + 1);
        }

        self.wrapped.format_instruction(instruction, f)
    }

    fn format_type(&self, ty: &Type, f: &mut fmt::Write) -> fmt::Result {
        self.wrapped.format_type(ty, f)
    }

    fn format_val(&self, val: &Value, f: &mut fmt::Write) -> fmt::Result {
        self.wrapped.format_val(val, f)
    }

    fn format_ref(&self, r: &Ref, f: &mut fmt::Write) -> fmt::Result {
        self.wrapped.format_ref(r, f)
    }

    fn format_field(&self, of_ty: &Type, field: FieldID, f: &mut fmt::Write) -> fmt::Result {
        self.wrapped.format_field(of_ty, field, f)
    }

    fn format_variant_case(&self, of_ty: &Type, tag: usize, f: &mut fmt::Write) -> fmt::Result {
        self.wrapped.format_variant_case(of_ty, tag, f)
    }
}
