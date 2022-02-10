use std::fmt;

use pas_common::span::*;
use pas_syn::{
    ast,
    ident::*,
};
use pas_syn::ast::IdentTypeName;

use crate::{context, result::*, Context, Decl, NameError, Symbol, ty::{Type, Specializable, typecheck_type}, NameContainer};

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum TypePattern {
    VariantCase {
        variant: Symbol,
        case: Ident,
        data_binding: Option<Ident>,
        span: Span,
    },
    NegatedVariantCase {
        variant: Symbol,
        case: Ident,
        span: Span,
    },
    Type {
        ty: Type,
        binding: Option<Ident>,
        span: Span,
    },
    NegatedType {
        ty: Type,
        span: Span,
    },
}

impl Spanned for TypePattern {
    fn span(&self) -> &Span {
        match self {
            TypePattern::VariantCase { span, .. } => span,
            TypePattern::NegatedVariantCase { span, .. } => span,
            TypePattern::Type { span, .. } => span,
            TypePattern::NegatedType { span, .. } => span,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct PatternBinding {
    pub ident: Ident,
    pub ty: Type,
}

impl TypePattern {
    // for an identPath (eg System.Option.Some) check if the last part is a declared variant
    // case and return the separate variant ident (eg System.Option) and case ident (eg Some)
    fn find_variant_case(
        path: &IdentPath,
        ctx: &Context,
    ) -> TypecheckResult<Option<(IdentPath, Ident)>> {
        let variant_parts = path.iter().cloned().take(path.as_slice().len() - 1);
        let stem_name = IdentPath::from_parts(variant_parts);
        let case_ident = path.last();

        match ctx.find_path(&stem_name) {
            Some(context::MemberRef::Value {
                     value: Decl::Type { ty, .. },
                     ..
                 }) if ty.as_variant().is_ok() => {
                let variant = ty.as_variant().unwrap();
                let variant_def = ctx.find_variant_def(&variant.qualified).unwrap();

                // check case with this ident exists
                if variant_def.case_position(case_ident).is_none() {
                    let err_span = path.first().span.to(&case_ident.span);

                    return Err(NameError::MemberNotFound {
                        base: NameContainer::Type(Type::Variant(Box::new(variant_def.name.clone()))),
                        member: case_ident.clone(),
                        span: err_span,
                    }
                        .into());
                }

                Ok(Some((
                    variant_def.name.qualified.clone(),
                    case_ident.clone(),
                )))
            }

            _ => Ok(None),
        }
    }

    // patterns referring to a typename can use a generic typename as the pattern, in which
    // case we infer the specialized type (or throw NotMatchable for generics we can't infer a
    // proper specialization for)
    fn find_pattern_ty(
        name: &ast::TypeName,
        span: &Span,
        expect_ty: &Type,
        ctx: &mut Context,
    ) -> TypecheckResult<Type> {
        let raw_ty = match name {
            ast::TypeName::Ident(IdentTypeName { ident, .. }) => ctx.find_type(ident)?.1.clone(),
            _ => typecheck_type(name, ctx)?,
        };

        let matchable_ty = if raw_ty.is_matchable() {
            raw_ty.infer_specialized_from_hint(expect_ty).cloned()
        } else {
            None
        };

        matchable_ty.ok_or_else(|| TypecheckError::NotMatchable {
            ty: raw_ty.clone(),
            span: span.clone(),
        })
    }

    // find_pattern_ty, but for variant type patterns
    fn find_pattern_variant(
        variant: &IdentPath,
        span: &Span,
        expect_ty: &Type,
        ctx: &mut Context,
    ) -> TypecheckResult<Symbol> {
        match expect_ty {
            expect_var @ Type::Variant(..) => {
                let variant_def = ctx.find_variant_def(variant)?;
                let var_ty = Type::Variant(Box::new(variant_def.name.clone()));

                var_ty.infer_specialized_from_hint(expect_var)
                    .map(|ty| match ty {
                        Type::Variant(v) => {
                            (**v).clone()
                        },
                        _ => unreachable!("should never infer a non-variant specialized type for a generic variant"),
                    })
                    .ok_or_else(|| TypecheckError::UnableToInferSpecialization {
                        generic_ty: var_ty.clone(),
                        hint_ty: expect_ty.clone(),
                        span: span.clone(),
                    })
            }

            _ => {
                let variant_def = ctx.find_variant_def(variant)?;
                // expect_ty is probably Nothing and we have to assume the type we find from
                // just the typename is right (if not, we'll get a type mismatch later)
                // todo: make sure we get a type mismatch later
                Ok(variant_def.name.clone())
            }
        }
    }

    pub fn typecheck(
        pattern: &ast::TypeNamePattern,
        expect_ty: &Type,
        ctx: &mut Context,
    ) -> TypecheckResult<TypePattern> {
        match pattern {
            // this pattern typename will never contain generic args (we can't parse those here),
            // so either this is a non-generic type or we'll infer a specialization from the
            // expression's expected type
            ast::TypeNamePattern::TypeName {
                name,
                kind,
                span,
            } => {
                let span = span.clone();

                match Self::find_variant_case(name, ctx)? {
                    Some((variant, case)) => {
                        let variant = Self::find_pattern_variant(&variant, &span, expect_ty, ctx)?;

                        match kind {
                            ast::TypeNamePatternKind::Is | ast::TypeNamePatternKind::IsWithBinding(..) => {
                                let data_binding = kind.binding().cloned();
                                Ok(TypePattern::VariantCase { variant, case, data_binding, span, })
                            }
                            ast::TypeNamePatternKind::IsNot => {
                                Ok(TypePattern::NegatedVariantCase { variant, case, span, })
                            }
                        }
                    }

                    None => {
                        let ty_name = ast::TypeName::Ident(IdentTypeName {
                            span: name.span().clone(),
                            indirection: 0,
                            type_args: None,
                            ident: name.clone(),
                        });

                        let ty = Self::find_pattern_ty(&ty_name, pattern.span(), expect_ty, ctx)?;

                        match kind {
                            ast::TypeNamePatternKind::Is | ast::TypeNamePatternKind::IsWithBinding(..) => {
                                let binding = kind.binding().cloned();
                                Ok(TypePattern::Type { ty, binding, span, })
                            }
                            ast::TypeNamePatternKind::IsNot => {
                                Ok(TypePattern::NegatedType { ty, span, })
                            }
                        }
                    }
                }
            },

            ast::TypeNamePattern::ExactType { name, kind, span } => {
                let span = span.clone();

                let ty = Self::find_pattern_ty(name, &span, expect_ty, ctx)?;

                match kind {
                    ast::TypeNamePatternKind::Is | ast::TypeNamePatternKind::IsWithBinding(..) => {
                        let binding = kind.binding().cloned();
                        Ok(TypePattern::Type { ty, binding, span, })
                    }
                    ast::TypeNamePatternKind::IsNot => {
                        Ok(TypePattern::NegatedType { ty, span, })
                    }
                }
            }
        }
    }

    pub fn bindings(&self, ctx: &mut Context) -> TypecheckResult<Vec<PatternBinding>> {
        match self {
            TypePattern::Type {
                ty,
                binding: Some(ident),
                ..
            } => {
                let binding = PatternBinding {
                    ident: ident.clone(),
                    ty: ty.clone(),
                };
                Ok(vec![binding])
            }

            TypePattern::VariantCase {
                variant,
                case,
                data_binding: Some(ident),
                ..
            } => {
                let variant_def = ctx.instantiate_variant(variant)?;
                let case_ty = variant_def.cases.iter()
                    .find_map(|c| if c.ident == *case {
                        c.data_ty.clone()
                    } else {
                        None
                    })
                    .expect("variant case pattern with a binding must always reference a case which has a data member");

                let binding = PatternBinding {
                    ty: case_ty,
                    ident: ident.clone(),
                };
                Ok(vec![binding])
            }

            _ => Ok(Vec::new()),
        }
    }
}

impl fmt::Display for TypePattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypePattern::Type { ty, binding, .. } => {
                write!(f, "{}", ty)?;
                if let Some(binding) = binding {
                    write!(f, " {}", binding)?;
                }
                Ok(())
            }

            TypePattern::NegatedType { ty, .. } => write!(f, "not {}", ty),

            TypePattern::VariantCase {
                variant,
                case,
                data_binding,
                ..
            } => {
                write!(f, "{}.{}", variant.qualified, case)?;
                if let Some(binding) = data_binding {
                    write!(f, " {}", binding)?;
                }
                Ok(())
            }

            TypePattern::NegatedVariantCase { variant, case, .. } => {
                write!(f, "not {}.{}", variant.qualified, case)
            }
        }
    }
}
