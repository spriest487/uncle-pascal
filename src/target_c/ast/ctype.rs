use std::fmt;

use types::Type;
use semantic::Scope;
use node::{
    FunctionArgModifier,
    Identifier,
};
use target_c::ast::{
    CallingConvention,
    Name,
};

#[derive(Debug, PartialEq, Clone)]
pub struct CArray {
    pub element: Box<CType>,
    pub count: u32,
}

#[derive(Debug, PartialEq, Clone)]
pub enum CType {
    Void,
    Named(Name),
    Pointer(Box<CType>),
    Ref(Box<CType>),
    Const(Box<CType>),
    Array(CArray),
    Function {
        return_type: Box<CType>,
        arg_types: Vec<CType>,
        calling_convention: CallingConvention,
    },
    Struct(Name),
}

impl From<Name> for CType {
    fn from(name: Name) -> Self {
        CType::Named(name)
    }
}

impl CType {
    pub fn translate(pascal_type: &Type, scope: &Scope) -> Self {
        match pascal_type {
            Type::Nil => unreachable!("nil type (c++ backend)"),
            Type::Byte => CType::from(Name::user_type(&Identifier::from("System.Byte"))),
            Type::Int32 => CType::from(Name::user_type(&Identifier::from("System.Int32"))),
            Type::UInt32 => CType::from(Name::user_type(&Identifier::from("System.UInt32"))),
            Type::Int64 => CType::from(Name::user_type(&Identifier::from("System.Int64"))),
            Type::UInt64 => CType::from(Name::user_type(&Identifier::from("System.UInt64"))),
            Type::NativeInt => CType::from(Name::user_type(&Identifier::from("System.NativeInt"))),
            Type::NativeUInt => CType::from(Name::user_type(&Identifier::from("System.NativeUInt"))),
            Type::Float64 => CType::from(Name::user_type(&Identifier::from("System.Float64"))),
            Type::Boolean => CType::from(Name::user_type(&Identifier::from("System.Boolean"))),
            Type::RawPointer => CType::from(Name::user_type(&Identifier::from("System.Pointer"))),
            Type::UntypedRef => CType::Void,
            Type::Pointer(target) => {
                CType::translate(target.as_ref(), scope)
                    .into_pointer()
            }
            Type::Function(sig) => {
                let return_type = sig.return_type.as_ref()
                    .map(|ty| CType::translate(ty, scope))
                    .unwrap_or_else(|| CType::Void);
                let arg_types = sig.args.iter()
                    .map(|arg| {
                        let arg_type_base = CType::translate(&arg.decl_type, scope);
                        match arg.modifier {
                            | Some(FunctionArgModifier::Var)
                            | Some(FunctionArgModifier::Out) =>
                                CType::Ref(Box::new(arg_type_base)),
                            | _ =>
                                arg_type_base
                        }
                    })
                    .collect::<Vec<_>>();

                CType::Function {
                    return_type: Box::new(return_type),
                    arg_types,
                    calling_convention: CallingConvention::from_modifiers(&sig.modifiers),
                }
            }
            Type::Class(name) => {
                let (class_id, _) = scope.get_class(name)
                    .expect("referenced class must exist");

                CType::Struct(Name::user_type(&class_id))
                    .into_pointer()
            }
            Type::Record(name) => {
                let (record_id, _) = scope.get_record(name)
                    .expect("referenced record must exist");

                CType::Struct(Name::user_type(&record_id))
            }

            Type::Enumeration(enum_id) => {
                let (enum_id, _) = scope.get_enumeration(enum_id)
                    .expect("referenced enumeration must exist");
                CType::Named(Name::user_type(&enum_id))
            }

            Type::AnyImplementation(_interface_id) => {
                CType::Struct(Name::internal_type("Object"))
                    .into_pointer()
            }

            Type::Set(set_id) => {
                let (set_id, _) = scope.get_set(set_id)
                    .expect("referenced set must exist");
                CType::Named(Name::user_type(&set_id))
            }

            Type::DynamicArray(_dynamic_array_type) => {
                unimplemented!("dynamic arrays (c++ backend)")
            }

            Type::Array(array) => {
                let element_type = CType::translate(array.element.as_ref(), scope);

                let mut base_array = CArray {
                    element: Box::new(element_type),
                    count: array.first_dim.elements(),
                };

                let multidim_array = array.rest_dims.iter().rev()
                    .fold(base_array, |last_rank, dim| {
                        CArray {
                            element: Box::new(CType::Array(CArray {
                                element: last_rank.element,
                                count: dim.elements(),
                            })),
                            ..last_rank
                        }
                    });

                CType::Array(multidim_array)
            }
        }
    }

    pub fn into_ref(self) -> Self {
        CType::Ref(Box::new(self))
    }

    pub fn into_const(self) -> Self {
        CType::Const(Box::new(self))
    }

    pub fn into_pointer(self) -> Self {
        CType::Pointer(Box::new(self))
    }

    pub fn is_void(&self) -> bool {
        match self {
            | CType::Void
            => true,
            | CType::Ref(base)
            | CType::Const(base)
            => base.is_void(),

            | _ => false
        }
    }
}

impl fmt::Display for CType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CType::Void =>
                write!(f, "void"),

            CType::Named(name) =>
                write!(f, "{}", name),

            CType::Ref(base) =>
                write!(f, "{}&", base),

            CType::Pointer(base) =>
                write!(f, "{}*", base),

            CType::Const(base) =>
                write!(f, "{} const", base),

            CType::Array(array) => {
                write!(f, "System_Internal_Array<{}[{}]>", array.element, array.count)
            }

            CType::Struct(name) => {
                write!(f, "struct {}", name)
            }

            CType::Function { return_type, arg_types, calling_convention } => {
                let args_list = arg_types.iter()
                    .map(|arg_type| arg_type.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");

                match calling_convention {
                    CallingConvention::Cdecl => write!(f, "System_Internal_Func_Cdecl<{}, {}>",
                           return_type, args_list),

                    CallingConvention::Stdcall => write!(f, "System_Internal_Func_Stdcall<{}, {}>",
                                                       return_type, args_list),
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use types::ArrayType;
    use semantic::IndexRange;

    #[test]
    fn multidim_array_has_correct_c_dims() {
        let pas_type = Type::Array(ArrayType {
            element: Box::new(Type::Int32),
            first_dim: IndexRange {
                from: 1,
                to: 2,
            },
            rest_dims: vec![
                IndexRange { from: 1, to: 3 },
                IndexRange { from: 1, to: 4 },
            ],
        });

        let scope = Scope::new_root();
        let c_type = CType::translate(&pas_type, &scope);

        let expected = CType::Array(CArray {
            count: 2,
            element: Box::new(CType::Array(CArray {
                count: 3,
                element: Box::new(CType::Array(CArray {
                    count: 4,
                    element: Box::new(CType::from(Name::internal_type("Int32"))),
                })),
            })),
        });

        assert_eq!(expected, c_type, "expected `{}` => `{}`, got `{}`",
                   pas_type,
                   expected,
                   c_type)
    }
}