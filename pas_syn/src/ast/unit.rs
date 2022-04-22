mod const_decl;
mod alias_decl;
mod unit_decl;

use std::fmt;
use crate::{ast::*, parse::prelude::*};
pub use self::{
    const_decl::ConstDecl,
    alias_decl::AliasDecl,
    unit_decl::{DeclMod, Visibility, UnitDecl, UseDecl},
};

#[derive(Clone, Debug)]
pub struct Unit<A: Annotation> {
    pub ident: IdentPath,

    pub iface_decls: Vec<UnitDecl<A>>,
    pub impl_decls: Vec<UnitDecl<A>>,
    pub init: Vec<Statement<A>>,
}

impl<A: Annotation> Unit<A> {
    pub fn all_decls(&self) -> impl Iterator<Item=(Visibility, &UnitDecl<A>)> {
        self.iface_decls
            .iter()
            .map(|decl| (Visibility::Interface, decl))
            .chain(self.impl_decls.iter()
                .map(|decl| (Visibility::Implementation, decl)))
    }

    pub fn func_decls(&self) -> impl Iterator<Item = (Visibility, &FunctionDecl<A>)> {
        self.all_decls()
            .filter_map(|(vis, decl)| match decl {
                UnitDecl::FunctionDecl { decl: func, .. } => Some((vis, func)),
                UnitDecl::FunctionDef { def: func_def, .. } => Some((vis, &func_def.decl)),
                _ => None,
            })
    }

    pub fn func_defs(&self) -> impl Iterator<Item = (Visibility, &FunctionDef<A>)> {
        self.all_decls()
            .filter_map(|(vis, decl)| match decl {
                UnitDecl::FunctionDef { def: func_def, .. } => Some((vis, func_def)),
                _ => None,
            })
    }

    pub fn type_decls<'a>(&self) -> impl Iterator<Item = (Visibility, &TypeDecl<A>)> {
        self
            .all_decls()
            .filter_map(|(vis, decl)| match decl {
                UnitDecl::Type { decl: ty, .. } => Some((vis, ty)),
                _ => None,
            })
    }
}

impl Unit<Span> {
    pub fn parse(tokens: &mut TokenStream, file_ident: IdentPath) -> ParseResult<Self> {
        let ident = match tokens.match_one_maybe(Keyword::Unit) {
            Some(_unit_kw) => {
                let ident = IdentPath::parse(tokens)?;
                tokens.match_one(Separator::Semicolon)?;
                ident
            },
            None => file_ident,
        };

        let mut iface_decls = Vec::new();
        let mut impl_decls = Vec::new();
        let mut init = Vec::new();

        let has_interface = parse_decls_section(Keyword::Interface, &mut iface_decls, tokens)?;
        let has_implementation = parse_decls_section(Keyword::Implementation, &mut impl_decls, tokens)?;

        let has_initialization = tokens.match_one_maybe(Keyword::Initialization).is_some();
        if has_initialization {
            let init_section = parse_init_section(tokens)?;
            init.extend(init_section);
        }

        if has_interface || has_implementation || has_initialization {
            // it's a structured unit, we expect nothing after the defined sections

            // if we get unexpected tokens here, we should suggest a semicolon after the last decl as a more
            // helpful error - we know there are tokens, and we know the only way for anything other than "end"
            // to be a legal token here is to separate them from the last decl with a semicolon
            let last_pos = tokens.context().clone();
            tokens.match_one(Keyword::End).map_err(|mut err| {
                if let ParseError::UnexpectedToken(..) = &err.err {
                    err.err = ParseError::ExpectedSeparator { span: last_pos, sep: Separator::Semicolon };
                }
                err
            })?;

            // allow the traditional period after the final end
            tokens.match_one_maybe(Operator::Member);
        } else {
            // no structured segments, it's a freeform unit - everything is in the interface
            // and we don't expect an end keyword after all decls/init
            let freeform_decls = UnitDecl::parse_seq(tokens)?;

            if freeform_decls.len() > 0 {
                iface_decls.extend(freeform_decls);

                if tokens.match_one_maybe(Separator::Semicolon).is_some() {
                    let init_after_decls = parse_init_section(tokens)?;
                    init.extend(init_after_decls);
                }
            }
        }

        Ok(Unit { ident, init, iface_decls, impl_decls })
    }
}

fn parse_decls_section(keyword: Keyword, out_decls: &mut Vec<UnitDecl<Span>>, tokens: &mut TokenStream) -> ParseResult<bool> {
    if !tokens.match_one_maybe(keyword).is_some() {
        return Ok(false);
    }

    let decls = UnitDecl::parse_seq(tokens)?;

    tokens.match_one(Separator::Semicolon)?;

    out_decls.extend(decls);

    Ok(true)
}

fn parse_unit_decl(tokens: &mut TokenStream) -> ParseResult<UnitDecl<Span>> {
    let decl_start = UnitDecl::start_matcher();

    let decl = match tokens.look_ahead().match_one(decl_start) {
        Some(tt) if tt.is_keyword(Keyword::Function) => {
            parse_unit_func_decl(tokens)?
        }

        Some(tt) if tt.is_keyword(Keyword::Type) => {
            let ty_decl = TypeDecl::parse(tokens)?;
            UnitDecl::Type {
                decl: ty_decl,
            }
        }

        Some(tt) if tt.is_keyword(Keyword::Uses) => {
            let uses_decl = UseDecl::parse(tokens)?;
            UnitDecl::Uses { decl: uses_decl }
        }

        Some(tt) if tt.is_keyword(Keyword::Const) => {
            let const_decl = ConstDecl::parse(tokens)?;
            UnitDecl::Const {
                decl: const_decl,
            }
        }

        Some(unexpected_tt) => {
            let err = ParseError::UnexpectedToken(Box::new(unexpected_tt), Some(UnitDecl::start_matcher()));
            return Err(TracedError::trace(err));
        },

        None => {
            let err = ParseError::UnexpectedEOF(UnitDecl::start_matcher(), tokens.context().clone());
            return Err(TracedError::trace(err));
        }
    };

    Ok(decl)
}

fn parse_unit_func_decl(tokens: &mut TokenStream) -> ParseResult<UnitDecl<Span>> {
    let func_decl = FunctionDecl::parse(tokens)?;

    let body_ahead = tokens.look_ahead()
        .match_sequence(Separator::Semicolon
            .and_then(FunctionDef::body_start_matcher()))
        .is_some();

    if body_ahead {
        tokens.match_one(Separator::Semicolon)?;

        let def = FunctionDef::parse_body_of_decl(func_decl, tokens)?;

        Ok(UnitDecl::FunctionDef {
            def,
        })
    } else {
        Ok(UnitDecl::FunctionDecl {
            decl: func_decl,
        })
    }
}

fn parse_init_section(tokens: &mut TokenStream) -> ParseResult<Vec<Statement<Span>>> {
    let stmts = Statement::parse_seq(tokens)?;

    // the last statement may be optionally terminated with a redundant separator
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