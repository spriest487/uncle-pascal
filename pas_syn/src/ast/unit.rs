mod alias_decl;
mod const_decl;
mod unit_decl;

pub use self::{
    alias_decl::AliasDecl,
    const_decl::{ConstDecl, ConstDeclItem},
    unit_decl::{DeclMod, UnitDecl, UseDecl, UseDeclItem, Visibility},
};
use crate::{
    ast::{Annotation, Block, FunctionDecl, FunctionDef, Stmt, TypeDecl, TypeDeclItem},
    parse::{Parse, ParseError, ParseResult, ParseSeq, TokenStream},
    IdentPath, Keyword, Operator, Separator, TokenTree,
};
use pas_common::{span::Span, TracedError};
use std::fmt;
use crate::parse::MatchOneOf;

#[derive(Eq, PartialEq, Hash, Copy, Clone, Debug)]
pub enum UnitKind {
    Program,
    Library,
    Unit,
}

#[derive(Clone, Debug)]
pub struct Unit<A: Annotation> {
    pub kind: UnitKind,

    pub ident: IdentPath,

    pub iface_decls: Vec<UnitDecl<A>>,
    pub impl_decls: Vec<UnitDecl<A>>,
    pub init: Vec<Stmt<A>>,
}

impl<A: Annotation> Unit<A> {
    pub fn all_decls(&self) -> impl Iterator<Item = (Visibility, &UnitDecl<A>)> {
        self.iface_decls
            .iter()
            .map(|decl| (Visibility::Interface, decl))
            .chain(
                self.impl_decls
                    .iter()
                    .map(|decl| (Visibility::Implementation, decl)),
            )
    }

    pub fn func_decls(&self) -> impl Iterator<Item = (Visibility, &FunctionDecl<A>)> {
        self.all_decls().filter_map(|(vis, decl)| match decl {
            UnitDecl::FunctionDecl { decl: func, .. } => Some((vis, func)),
            UnitDecl::FunctionDef { def: func_def, .. } => Some((vis, &func_def.decl)),
            _ => None,
        })
    }

    pub fn func_defs(&self) -> impl Iterator<Item = (Visibility, &FunctionDef<A>)> {
        self.all_decls().filter_map(|(vis, decl)| match decl {
            UnitDecl::FunctionDef { def: func_def, .. } => Some((vis, func_def)),
            _ => None,
        })
    }

    pub fn type_decls<'a>(&self) -> impl Iterator<Item = (Visibility, &TypeDecl<A>)> {
        self.all_decls().filter_map(|(vis, decl)| match decl {
            UnitDecl::Type { decl: ty, .. } => Some((vis, ty)),
            _ => None,
        })
    }

    pub fn type_decl_items(&self) -> impl Iterator<Item = (Visibility, &TypeDeclItem<A>)> {
        self.type_decls()
            .flat_map(|(vis, decl)| decl.items.iter().map(move |item| (vis, item)))
    }
}

impl Unit<Span> {
    pub fn parse(tokens: &mut TokenStream, file_ident: IdentPath) -> ParseResult<Self> {
        let unit_kind_kw_match = Keyword::Unit.or(Keyword::Program).or(Keyword::Library);

        let (unit_kind, ident) = match tokens.match_one_maybe(unit_kind_kw_match) {
            Some(TokenTree::Keyword { kw, .. }) => {
                let ident = IdentPath::parse(tokens)?;
                tokens.match_one(Separator::Semicolon)?;

                let kind = match kw {
                    Keyword::Program => UnitKind::Program,
                    Keyword::Library => UnitKind::Library,
                    _ => UnitKind::Unit,
                };
                (kind, ident)
            },

            _ => (UnitKind::Unit, file_ident),
        };

        let mut iface_decls = Vec::new();
        let mut impl_decls = Vec::new();
        let mut init = Vec::new();

        if unit_kind == UnitKind::Program {
            let decls = UnitDecl::parse_seq(Keyword::Implementation, tokens)?;

            if !decls.is_empty() {
                tokens.match_one(Separator::Semicolon)?;
            }

            impl_decls.extend(decls);

            let main_block = Block::parse(tokens)?;
            init.push(Stmt::Block(Box::new(main_block)));

            // allow the traditional period after the final end
            tokens.match_one_maybe(Operator::Period);
        } else {
            let has_interface = parse_decls_section(Keyword::Interface, &mut iface_decls, tokens)?;
            let has_implementation =
                parse_decls_section(Keyword::Implementation, &mut impl_decls, tokens)?;

            let has_initialization = tokens.match_one_maybe(Keyword::Initialization).is_some();
            if has_initialization {
                let init_section = parse_init_section(tokens)?;
                init.extend(init_section);
            }

            if has_interface || has_implementation || has_initialization {
                // it's a structured unit, we expect nothing after the defined sections
                tokens.match_one(Keyword::End)?;

                // allow the traditional period after the final end
                tokens.match_one_maybe(Operator::Period);
            } else {
                // no structured segments, it's a freeform unit - everything is implementation
                // and we don't expect an end keyword after all decls/init
                let freeform_decls = if UnitDecl::has_more(&iface_decls, &mut tokens.look_ahead()) {
                    UnitDecl::parse_seq(Keyword::Implementation, tokens)?
                } else {
                    vec![]
                };

                if freeform_decls.len() > 0 {
                    iface_decls.extend(freeform_decls);

                    if tokens.match_one_maybe(Separator::Semicolon).is_some() {
                        let init_after_decls = parse_init_section(tokens)?;
                        init.extend(init_after_decls);
                    }
                } else {
                    let init_after_decls = parse_init_section(tokens)?;
                    init.extend(init_after_decls);
                }
            }
        }

        Ok(Unit {
            kind: unit_kind,
            ident,
            init,
            iface_decls,
            impl_decls,
        })
    }
}

fn parse_decls_section(
    keyword: Keyword,
    out_decls: &mut Vec<UnitDecl<Span>>,
    tokens: &mut TokenStream,
) -> ParseResult<bool> {
    if !tokens.match_one_maybe(keyword).is_some() {
        return Ok(false);
    }

    let decls = UnitDecl::parse_seq(keyword, tokens)?;

    if !decls.is_empty() {
        tokens.match_one(Separator::Semicolon)?;
    }

    out_decls.extend(decls);

    Ok(true)
}

fn parse_unit_decl(tokens: &mut TokenStream, part_kw: Keyword) -> ParseResult<UnitDecl<Span>> {
    let decl_start = UnitDecl::start_matcher();

    let decl = match tokens.look_ahead().match_one(decl_start) {
        Some(tt) if tt.is_keyword(Keyword::Function) || tt.is_keyword(Keyword::Procedure) => {
            parse_unit_func_decl(part_kw, tokens)?
        },

        Some(tt) if tt.is_keyword(Keyword::Type) => UnitDecl::Type {
            decl: TypeDecl::parse(tokens)?,
        },

        Some(tt) if tt.is_keyword(Keyword::Uses) => UnitDecl::Uses {
            decl: UseDecl::parse(tokens)?,
        },

        Some(tt) if tt.is_keyword(Keyword::Const) => UnitDecl::Const {
            decl: ConstDecl::parse(tokens)?,
        },

        Some(unexpected_tt) => {
            let err = ParseError::UnexpectedToken(
                Box::new(unexpected_tt),
                Some(UnitDecl::start_matcher()),
            );
            return Err(TracedError::trace(err));
        },

        None => {
            let err =
                ParseError::UnexpectedEOF(UnitDecl::start_matcher(), tokens.context().clone());
            return Err(TracedError::trace(err));
        },
    };

    Ok(decl)
}

fn parse_unit_func_decl(part_kw: Keyword, tokens: &mut TokenStream) -> ParseResult<UnitDecl<Span>> {
    let func_decl = FunctionDecl::parse(tokens)?;

    let body_ahead = if part_kw == Keyword::Interface {
        // interface funcs - never expect a body, unless the function is marked `inline`
        func_decl.mods.iter().any(|decl_mod| match decl_mod {
            DeclMod::Inline(..) => true,
            _ => false,
        })
    } else {
        // implementation funcs - always expect a body, unless the function is marked `forward`
        !func_decl.mods.iter().any(|decl_mod| match decl_mod {
            DeclMod::Forward(..) => true,
            _ => false,
        })
    };

    if body_ahead {
        tokens.match_one(Separator::Semicolon)?;

        let def = FunctionDef::parse_body_of_decl(func_decl, tokens);
        if def.is_err() {
            eprintln!("failed on {}", tokens.current().unwrap().span());
        }
        let def = def?;

        Ok(UnitDecl::FunctionDef { def })
    } else {
        Ok(UnitDecl::FunctionDecl { decl: func_decl })
    }
}

fn parse_init_section(tokens: &mut TokenStream) -> ParseResult<Vec<Stmt<Span>>> {
    let stmts = Stmt::parse_seq(tokens)?;

    // the last stmt may be optionally terminated with a redundant separator
    if stmts.len() > 0 {
        tokens.match_one_maybe(Separator::Semicolon);
    }

    Ok(stmts)
}

impl<A: Annotation> fmt::Display for Unit<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "unit {};", self.ident)?;

        writeln!(f, "interface")?;
        writeln!(f)?;

        for decl in &self.iface_decls {
            writeln!(f, "{};", decl)?;
        }
        writeln!(f)?;

        writeln!(f, "implementation")?;
        writeln!(f)?;

        for decl in &self.impl_decls {
            writeln!(f, "{};", decl)?;
        }
        writeln!(f)?;

        writeln!(f, "initialization")?;
        writeln!(f)?;

        for init_stmt in &self.init {
            writeln!(f, "{};", init_stmt)?;
        }
        writeln!(f)?;

        writeln!(f, "end")?;

        Ok(())
    }
}
