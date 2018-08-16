use std::fmt;

use types::Type;
use semantic::Scope;
use node::FunctionArgModifier;
use target_c::writer::identifier_to_c;

#[derive(Debug, PartialEq)]
pub struct CArray {
    pub element: Box<CType>,
    pub count: u32,
}

#[derive(Debug, PartialEq)]
pub enum CType {
    Void,
    Named(String),
    Pointer(Box<CType>),
    Ref(Box<CType>),
    Const(Box<CType>),
    Array(CArray),
    Function {
        return_type: Box<CType>,
        arg_types: Vec<CType>,
    },
}

impl<'a> From<&'a str> for CType {
    fn from(s: &str) -> Self {
        CType::Named(s.to_string())
    }
}

impl CType {
    pub fn from_pascal(pascal_type: &Type, scope: &Scope) -> Self {
        match pascal_type {
            Type::Nil => panic!("cannot output `nil` as a type in C"),
            Type::Byte => CType::from("System_Byte"),
            Type::Int32 => CType::from("System_Int32"),
            Type::UInt32 => CType::from("System_UInt32"),
            Type::Int64 => CType::from("System_Int64"),
            Type::UInt64 => CType::from("System_UInt64"),
            Type::NativeInt => CType::from("System_NativeInt"),
            Type::NativeUInt => CType::from("System_NativeUInt"),
            Type::Float64 => CType::from("System_Float64"),
            Type::Boolean => CType::from("System_Boolean"),
            Type::RawPointer => CType::from("System_Pointer"),
            Type::UntypedRef => CType::Void,
            Type::Pointer(target) => {
                CType::from_pascal(target.as_ref(), scope)
                    .into_pointer()
            }
            Type::Function(sig) => {
                let return_type = sig.return_type.as_ref()
                    .map(|ty| CType::from_pascal(ty, scope))
                    .unwrap_or_else(|| CType::Void);
                let arg_types = sig.args.iter()
                    .map(|arg| {
                        let arg_type_base = CType::from_pascal(&arg.decl_type, scope);
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
                }
            }
            Type::Class(name) => {
                let (class_id, _) = scope.get_class(name)
                    .expect("referenced class must exist");

                CType::Named(identifier_to_c(&class_id))
                    .into_pointer()
            }
            Type::Record(name) => {
                let (record_id, _) = scope.get_record(name)
                    .expect("referenced record must exist");

                CType::Named(identifier_to_c(&record_id))
            }

            Type::Enumeration(enum_id) => {
                let (enum_id, _) = scope.get_enumeration(enum_id)
                    .expect("referenced enumeration must exist");
                CType::Named(identifier_to_c(&enum_id))
            }

            Type::Set(set_id) => {
                let (set_id, _) = scope.get_set(set_id)
                    .expect("referenced set must exist");
                CType::Named(identifier_to_c(&set_id))
            }

            Type::DynamicArray(dynamic_array_type) => {
                let element_type = CType::from_pascal(&dynamic_array_type.element, scope);
                CType::Named(format!("std::shared_ptr<{}[]>", element_type))
            }

            Type::Array(array) => {
                let element_type = CType::from_pascal(array.element.as_ref(), scope);

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

            CType::Function { return_type, arg_types } => {
                let args_list = arg_types.iter()
                    .map(|arg_type| arg_type.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");

                write!(f, "System_Internal_Func<{}, {}>", return_type, args_list)
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

        let scope = Scope::default();
        let c_type = CType::from_pascal(&pas_type, &scope);

        let expected = CType::Array(CArray {
            count: 2,
            element: Box::new(CType::Array(CArray {
                count: 3,
                element: Box::new(CType::Array(CArray {
                    count: 4,
                    element: Box::new(CType::from("System_Int32")),
                })),
            })),
        });

        assert_eq!(expected, c_type, "expected `{}` => `{}`, got `{}`",
                   pas_type,
                   expected,
                   c_type)
    }
}