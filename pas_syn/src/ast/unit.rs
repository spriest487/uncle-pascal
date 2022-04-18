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

    pub decls: Vec<UnitDecl<A>>,
    pub init: Vec<Statement<A>>,
}

impl<A: Annotation> Unit<A> {
    pub fn func_decls(&self) -> impl Iterator<Item = &FunctionDecl<A>> {
        self.decls.iter().filter_map(|decl| match decl {
            UnitDecl::FunctionDecl { decl: func, .. } => Some(func),
            UnitDecl::FunctionDef { def: func_def, .. } => Some(&func_def.decl),
            _ => None,
        })
    }

    pub fn func_defs(&self) -> impl Iterator<Item = &FunctionDef<A>> {
        self.decls.iter().filter_map(|decl| match decl {
            UnitDecl::FunctionDef { def: func_def, .. } => Some(func_def),
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
    pub fn parse(tokens: &mut TokenStream, file_ident: IdentPath) -> ParseResult<Self> {
        let ident = match tokens.match_one_maybe(Keyword::Unit) {
            Some(_unit_kw) => {
                let ident = IdentPath::parse(tokens)?;
                tokens.match_one(Separator::Semicolon)?;
                ident
            },
            None => file_ident,
        };

        let mut decls = Vec::new();
        let mut init = Vec::new();

        let has_interface = tokens.match_one_maybe(Keyword::Interface).is_some();
        if has_interface {
            let interface_decls = parse_unit_decls(tokens, Visibility::Interface)?;
            decls.extend(interface_decls);
        }

        let has_implementation = tokens.match_one_maybe(Keyword::Implementation).is_some();
        if has_implementation {
            let interface_decls = parse_unit_decls(tokens, Visibility::Implementation)?;
            decls.extend(interface_decls);
        }

        let has_initialization = tokens.match_one_maybe(Keyword::Initialization).is_some();
        if has_initialization {
            let init_section = parse_init_section(tokens)?;
            init.extend(init_section)
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

            // unit can optionally be terminated by a full stop, for tradition's sake
            tokens.match_one_maybe(Operator::Member);
        } else {
            // no structured segments, it's a freeform unit - everything is in the interface
            // and we don't expect an end keyword after all decls/init
            let freeform_decls = parse_unit_decls(tokens, Visibility::Interface)?;
            decls.extend(freeform_decls);

            let init_after_decls = parse_init_section(tokens)?;

            init.extend(init_after_decls);
        }

        Ok(Unit { ident, init, decls })
    }
}

fn unit_decl_start_matcher() -> Matcher {
    Keyword::Function
        .or(Keyword::Uses)
        .or(Keyword::Type)
        .or(Keyword::Const)
}

fn parse_unit_decls(tokens: &mut TokenStream, visibility: Visibility) -> ParseResult<Vec<UnitDecl<Span>>> {
    let decl_start_matcher = unit_decl_start_matcher();

    let mut decls = Vec::new();
    loop {
        match parse_unit_decl(tokens, visibility)? {
            Some(decl) => decls.push(decl),
            None => break,
        }

        if tokens.match_one_maybe(Separator::Semicolon).is_none() {
            break;
        }

        if tokens.look_ahead().match_one(decl_start_matcher.clone()).is_none() {
            break;
        }
    }

    Ok(decls)
}

// can't use match_separated here because it doesn't play nicely
// with the fact that type decls are also semicolon-separated lists
fn parse_unit_decl(tokens: &mut TokenStream, visibility: Visibility) -> ParseResult<Option<UnitDecl<Span>>> {
    let decl_start = unit_decl_start_matcher();

    let decl = match tokens.look_ahead().match_one(decl_start) {
        Some(tt) if tt.is_keyword(Keyword::Function) => {
            let func_decl = parse_unit_func_decl(tokens, visibility)?;

            Some(func_decl)
        }

        Some(tt) if tt.is_keyword(Keyword::Type) => {
            let ty_decl = TypeDecl::parse(tokens)?;
            Some(UnitDecl::Type {
                decl: ty_decl,
                visibility,
            })
        }

        Some(tt) if tt.is_keyword(Keyword::Uses) => {
            let uses_decl = UseDecl::parse(tokens)?;
            Some(UnitDecl::Uses { decl: uses_decl })
        }

        Some(tt) if tt.is_keyword(Keyword::Const) => {
            let const_decl = ConstDecl::parse(tokens)?;
            Some(UnitDecl::Const {
                decl: const_decl,
                visibility,
            })
        }

        _ => None,
    };

    Ok(decl)
}

fn parse_unit_func_decl(tokens: &mut TokenStream, visibility: Visibility) -> ParseResult<UnitDecl<Span>> {
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
            visibility,
        })
    } else {
        Ok(UnitDecl::FunctionDecl {
            decl: func_decl,
            visibility,
        })
    }
}

fn parse_init_section(tokens: &mut TokenStream) -> ParseResult<Vec<Statement<Span>>> {
    let stmts = Statement::parse_seq(tokens)?;

    // the last statement may be optionally terminated with a redundant separator
    tokens.match_one_maybe(Separator::Semicolon);

    Ok(stmts)
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