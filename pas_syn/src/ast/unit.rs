use crate::{ast::*, parse::prelude::*};
use std::fmt;

#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq)]
pub enum Visibility {
    Exported,
    Private,
}

#[derive(Clone, Debug)]
pub struct Unit<A: Annotation> {
    pub ident: Ident,

    pub decls: Vec<UnitDecl<A>>,
    pub init: Vec<Statement<A>>,
}

impl<A: Annotation> Unit<A> {
    pub fn func_decls(&self) -> impl Iterator<Item = &FunctionDecl<A>> {
        self.decls.iter().filter_map(|decl| match decl {
            UnitDecl::FunctionDecl { decl: func, .. } => Some(func),
            UnitDecl::FunctionDef { decl: func_def, .. } => Some(&func_def.decl),
            _ => None,
        })
    }

    pub fn func_defs(&self) -> impl Iterator<Item = &FunctionDef<A>> {
        self.decls.iter().filter_map(|decl| match decl {
            UnitDecl::FunctionDef { decl: func_def, .. } => Some(func_def),
            _ => None,
        })
    }

    pub fn type_decls<'a>(&self) -> impl Iterator<Item = &TypeDecl<A>> {
        self.decls.iter().filter_map(|decl| match decl {
            UnitDecl::Type { decl: ty, .. } => Some(ty),
            _ => None,
        })
    }
}

impl Unit<Span> {
    pub fn parse(tokens: &mut TokenStream, ident: Ident) -> ParseResult<Self> {
        let mut decls = Vec::new();

        // can't use match_separated here because it doesn't play nicely
        // with the fact that type decls are also semicolon-separated lists
        loop {
            let export_kw = tokens.match_one_maybe(Keyword::Export);
            let visibility = match &export_kw {
                Some(..) => Visibility::Exported,
                None => Visibility::Private,
            };

            let exportable_decl_kw = Keyword::Function.or(Keyword::Type);

            let decl_start = match export_kw {
                Some(..) => exportable_decl_kw,
                None => exportable_decl_kw.or(Keyword::Uses),
            };

            match tokens.look_ahead().match_one(decl_start) {
                Some(TokenTree::Keyword {
                    kw: Keyword::Function,
                    ..
                }) => {
                    let func_decl = FunctionDecl::parse(tokens)?;

                    let begin_next = tokens
                        .look_ahead()
                        .match_one(DelimiterPair::BeginEnd)
                        .is_some();
                    if begin_next {
                        let body = Block::parse(tokens)?;

                        decls.push(UnitDecl::FunctionDef {
                            decl: FunctionDef {
                                span: func_decl.span().to(body.span()),
                                decl: func_decl,
                                body,
                            },
                            visibility,
                        });
                    } else {
                        decls.push(UnitDecl::FunctionDecl {
                            decl: func_decl,
                            visibility,
                        });
                    }
                }

                Some(TokenTree::Keyword {
                    kw: Keyword::Type, ..
                }) => {
                    let ty_decl = TypeDecl::parse(tokens)?;
                    decls.push(UnitDecl::Type {
                        decl: ty_decl,
                        visibility,
                    });
                }

                Some(TokenTree::Keyword {
                    kw: Keyword::Uses, ..
                }) => {
                    let uses_decl = UseDecl::parse(tokens)?;
                    decls.push(UnitDecl::Uses { decl: uses_decl });
                }

                _ => break,
            }

            if !tokens.match_one_maybe(Separator::Semicolon).is_some() {
                break;
            }
        }

        let init = tokens.match_separated(Separator::Semicolon, |_, tokens| {
            let stmt_next = tokens
                .look_ahead()
                .match_one(statement_start_matcher())
                .is_some();

            if !stmt_next {
                Ok(Generate::Break)
            } else {
                let stmt = Statement::parse(tokens)?;
                Ok(Generate::Yield(stmt))
            }
        })?;

        Ok(Unit { ident, init, decls })
    }
}

impl<A: Annotation> fmt::Display for Unit<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for decl in &self.decls {
            writeln!(f, "{};", decl)?;
        }

        writeln!(f)?;

        for init_stmt in &self.init {
            writeln!(f, "{};", init_stmt)?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub enum UnitDecl<A: Annotation> {
    FunctionDecl {
        decl: FunctionDecl<A>,
        visibility: Visibility,
    },
    FunctionDef {
        decl: FunctionDef<A>,
        visibility: Visibility,
    },
    Type {
        decl: TypeDecl<A>,
        visibility: Visibility,
    },
    Uses {
        decl: UseDecl,
    },
}

impl<A: Annotation> Spanned for UnitDecl<A> {
    fn span(&self) -> &Span {
        match self {
            UnitDecl::FunctionDecl {
                decl: func_decl, ..
            } => func_decl.span(),
            UnitDecl::FunctionDef { decl: func_def, .. } => func_def.span(),
            UnitDecl::Type {
                decl: type_decl, ..
            } => type_decl.span(),
            UnitDecl::Uses { decl: use_decl } => use_decl.span(),
        }
    }
}

impl<A: Annotation> fmt::Display for UnitDecl<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            UnitDecl::FunctionDecl {
                decl: func_decl, ..
            } => write!(f, "{}", func_decl),
            UnitDecl::FunctionDef { decl: func_def, .. } => write!(f, "{}", func_def),
            UnitDecl::Type { decl: ty_decl, .. } => write!(f, "{}", ty_decl),
            UnitDecl::Uses { decl: uses } => write!(f, "{}", uses),
        }
    }
}

#[derive(Clone, Debug)]
pub struct UseDecl {
    pub units: Vec<Ident>,
    pub span: Span,
}

impl Spanned for UseDecl {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl fmt::Display for UseDecl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "uses ")?;
        for (i, unit) in self.units.iter().enumerate() {
            if i > 0 {
                write!(f, ",")?;
            }
            write!(f, "{}", unit)?;
        }
        Ok(())
    }
}

impl UseDecl {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let kw = tokens.match_one(Keyword::Uses)?;
        let units = tokens.match_separated(Separator::Comma, |_, tokens| {
            let unit_ident = tokens.match_one(Matcher::AnyIdent)?;
            Ok(Generate::Yield(unit_ident.into_ident().unwrap()))
        })?;

        if units.is_empty() {
            return Err(TracedError::trace(match tokens.look_ahead().next() {
                None => ParseError::UnexpectedEOF(Matcher::AnyIdent, kw.span().clone()),
                Some(x) => ParseError::UnexpectedToken(Box::new(x), Some(Matcher::AnyIdent)),
            }));
        }

        let span = kw.span().to(units.last().unwrap().span());

        Ok(Self { units, span })
    }
}

#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub enum DeclMod {
    External { src: String, span: Span },
}

impl DeclMod {
    const EXTERNAL_WORD: &'static str = "external";

    pub fn keyword(&self) -> &str {
        match self {
            DeclMod::External { .. } => Self::EXTERNAL_WORD,
        }
    }

    fn parse_external(tokens: &mut TokenStream) -> ParseResult<DeclMod> {
        let kw = tokens.match_one(Self::EXTERNAL_WORD)?.into_ident().unwrap();
        let src = tokens.match_one(Matcher::AnyLiteralString)?;

        Ok(DeclMod::External {
            span: kw.span().to(src.span()),
            src: src.as_literal_string().unwrap().to_string(),
        })
    }

    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Vec<DeclMod>> {
        let mut mods: Vec<DeclMod> = Vec::new();

        let mod_seq = Separator::Semicolon.and_then(Self::EXTERNAL_WORD);

        while let Some(mut seq_tokens) = tokens.look_ahead().match_sequence(mod_seq.clone()) {
            tokens.advance(1);

            let word_token = seq_tokens.remove(1).into_ident().unwrap();

            let new_mod = match word_token.name.as_str() {
                Self::EXTERNAL_WORD => Self::parse_external(tokens)?,

                _ => unreachable!("bad modified contextual keyword"),
            };

            let existing = mods.iter().find(|m| m.keyword() == new_mod.keyword());
            if let Some(existing) = existing {
                return Err(TracedError::trace(ParseError::DuplicateModifier {
                    new: new_mod,
                    existing: existing.clone(),
                }));
            }

            mods.push(new_mod);
        }

        Ok(mods)
    }
}

impl fmt::Display for DeclMod {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            DeclMod::External { src, .. } => write!(f, "external '{}'", src),
        }
    }
}

impl Spanned for DeclMod {
    fn span(&self) -> &Span {
        match self {
            DeclMod::External { span, .. } => span,
        }
    }
}
