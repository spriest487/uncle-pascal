use std::fmt;

use types::{
    Type,
    Reference,
    ParameterizedName,
};
use semantic::Scope;
use node::FunctionArgModifier;
use target_c::ast::{
    CallingConvention,
    TranslationUnit,
    Name,
    TranslationResult,
};

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct CArray {
    pub element: Box<CType>,
    pub count: usize,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
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
    pub fn translate(pascal_type: &Type, scope: &Scope, unit: &mut TranslationUnit) -> TranslationResult<Self> {
        match pascal_type {
            Type::Nil => unreachable!("nil type (c++ backend)"),
            Type::Generic(_) => unreachable!("unresolved generic type {} (c++ backend)", pascal_type),

            Type::Byte => Ok(CType::from(Name::user_type(&ParameterizedName::new_simple("System.Byte")))),
            Type::Int32 => Ok(CType::from(Name::user_type(&ParameterizedName::new_simple("System.Int32")))),
            Type::UInt32 => Ok(CType::from(Name::user_type(&ParameterizedName::new_simple("System.UInt32")))),
            Type::Int64 => Ok(CType::from(Name::user_type(&ParameterizedName::new_simple("System.Int64")))),
            Type::UInt64 => Ok(CType::from(Name::user_type(&ParameterizedName::new_simple("System.UInt64")))),
            Type::NativeInt => Ok(CType::from(Name::user_type(&ParameterizedName::new_simple("System.NativeInt")))),
            Type::NativeUInt => Ok(CType::from(Name::user_type(&ParameterizedName::new_simple("System.NativeUInt")))),
            Type::Float64 => Ok(CType::from(Name::user_type(&ParameterizedName::new_simple("System.Float64")))),
            Type::Boolean => Ok(CType::from(Name::user_type(&ParameterizedName::new_simple("System.Boolean")))),
            Type::RawPointer => Ok(CType::from(Name::user_type(&ParameterizedName::new_simple("System.Pointer")))),
            Type::UntypedRef => Ok(CType::Void),
            Type::Pointer(target) => {
                CType::translate(target.as_ref(), scope, unit)
                    .map(|ty| ty.into_pointer())
            }
            Type::Function(sig) => {
                let return_type = sig.return_type.as_ref()
                    .map(|ty| CType::translate(ty, scope, unit))
                    .unwrap_or_else(|| Ok(CType::Void))?;

                let arg_types = sig.args.iter()
                    .map(|arg| {
                        let arg_type_base = CType::translate(&arg.decl_type, scope, unit)?;

                        match arg.modifier {
                            | Some(FunctionArgModifier::Var)
                            | Some(FunctionArgModifier::Out) =>
                                Ok(CType::Ref(Box::new(arg_type_base))),
                            | _ =>
                                Ok(arg_type_base)
                        }
                    })
                    .collect::<TranslationResult<Vec<_>>>()?;

                Ok(CType::Function {
                    return_type: Box::new(return_type),
                    arg_types,
                    calling_convention: CallingConvention::from_modifiers(&sig.modifiers),
                })
            }

            Type::Record(name) => {
                let (name, _) = scope.get_record_specialized(name)
                    .expect("referenced record must exist");

                let decl = unit.struct_decl(&name)?;

                Ok(CType::Struct(decl.decl.name.clone()
                    .expect("record structs are always named")))
            }

            Type::Enumeration(enum_id) => {
                let (enum_id, _) = scope.get_enumeration(enum_id)
                    .expect("referenced enumeration must exist");

                Ok(CType::Named(Name::user_type(&ParameterizedName::new_simple(enum_id.clone()))))
            }

            Type::Set(set_id) => {
                let (set_id, _) = scope.get_set(set_id)
                    .expect("referenced set must exist");

                Ok(CType::Named(Name::user_type(&ParameterizedName::new_simple(set_id.clone()))))
            }

            Type::Array(array) => {
                let element_type = CType::translate(array.element.as_ref(), scope, unit)?;

                let mut base_array = CArray {
                    element: Box::new(element_type),
                    count: array.total_elements(),
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

                Ok(CType::Array(multidim_array))
            }

            | Type::Ref(ref_type)
            | Type::WeakRef(ref_type)
            => Self::translate_reference(ref_type, scope, unit)
        }
    }

    fn translate_reference(ref_type: &Reference,
                           scope: &Scope,
                           unit: &mut TranslationUnit)
                           -> TranslationResult<Self> {
        match ref_type {
            Reference::Class(name) => {
                let (name, _) = scope.get_class_specialized(name)
                    .expect("referenced class must exist");

                let decl = unit.struct_decl(&name)?;

                Ok(CType::Struct(decl.decl.name.clone()
                    .expect("class structs are always named"))
                    .into_pointer())
            }

            Reference::Interface(_interface_id) => {
                Ok(CType::Struct(Name::internal_type("Object"))
                    .into_pointer())
            }

            Reference::DynamicArray(_dynamic_array_type) => {
                unimplemented!("dynamic arrays (c++ backend)")
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
        let c_type = CType::translate(&pas_type, &scope, &mut TranslationUnit::new())
            .unwrap();

        let expected = CType::Array(CArray {
            count: 2,
            element: Box::new(CType::Array(CArray {
                count: 3,
                element: Box::new(CType::Array(CArray {
                    count: 4,
                    element: Box::new(CType::from(Name::user_type(&ParameterizedName::new_simple("System.Int32")))),
                })),
            })),
        });

        assert_eq!(expected, c_type, "expected `{}` => `{}`, got `{}`",
                   pas_type,
                   expected,
                   c_type)
    }
}