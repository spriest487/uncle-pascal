mod alias_decl;
mod const_decl;
mod unit_decl;
mod decl_mod;
mod use_decl;

pub use self::alias_decl::*;
pub use self::const_decl::*;
pub use self::decl_mod::*;
pub use self::unit_decl::*;
pub use self::use_decl::*;
use crate::ast::Annotation;
use crate::ast::Block;
use crate::ast::FunctionDecl;
use crate::ast::FunctionDef;
use crate::ast::IdentPath;
use crate::ast::Stmt;
use crate::ast::TypeDecl;
use crate::ast::TypeDeclItem;
pub use crate::parse::MatchOneOf;
use crate::parse::Parse;
use crate::parse::ParseError;
use crate::parse::ParseResult;
use crate::parse::ParseSeq;
use crate::parse::TokenStream;
use crate::Keyword;
use crate::Operator;
use crate::Separator;
use crate::TokenTree;
use common::span::Span;
use common::TracedError;
use std::fmt;
use std::rc::Rc;

#[derive(Eq, PartialEq, Hash, Copy, Clone, Debug)]
pub enum UnitKind {
    Program,
    Library,
    Unit,
}

#[derive(Clone, Debug)]
pub struct Unit<A: Annotation = Span> {
    pub kind: UnitKind,

    pub ident: IdentPath,

    pub iface_decls: Vec<UnitDecl<A>>,
    pub impl_decls: Vec<UnitDecl<A>>,

    pub init: Option<InitBlock<A>>,
}

#[derive(Clone, Debug)]
pub struct InitBlock<A: Annotation = Span> {
    pub keyword_span: Span,
    pub body: Vec<Stmt<A>>,
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

    pub fn func_decls(&self) -> impl Iterator<Item = (Visibility, &Rc<FunctionDecl<A>>)> {
        self.all_decls().filter_map(|(vis, decl)| match decl {
            UnitDecl::FunctionDecl { decl: func, .. } => Some((vis, func)),
            UnitDecl::FunctionDef { def: func_def, .. } => Some((vis, &func_def.decl)),
            _ => None,
        })
    }

    pub fn func_defs(&self) -> impl Iterator<Item = (Visibility, &Rc<FunctionDef<A>>)> {
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
        let unit_kind_kw_match = Keyword::Unit | Keyword::Program | Keyword::Library;

        let (unit_kind, ident) = match tokens.match_one_maybe(unit_kind_kw_match.clone()) {
            Some(TokenTree::Keyword { kw, .. }) => {
                let ident = IdentPath::parse(tokens)?;
                tokens.match_one(Separator::Semicolon)?;
                
                if file_ident != ident {
                    let err = ParseError::InvalidUnitFilename(ident.path_span());
                    return Err(TracedError::trace(err));
                }

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
        let mut init = None;

        if unit_kind == UnitKind::Program {
            let decls = UnitDecl::parse_seq(Keyword::Implementation, tokens)?;

            if !decls.is_empty() {
                tokens.match_one(Separator::Semicolon)?;
            }

            impl_decls.extend(decls);

            // instead of a separate init block, program units always have a "main" block
            // after any decls with the usual begin/end keywords
            let main_block = Block::parse(tokens)?;
            
            init = Some(InitBlock {
                keyword_span: main_block.begin.clone(),
                body: vec![Stmt::Block(Box::new(main_block))],
            });
        } else {
            let has_interface = parse_decls_section(Keyword::Interface, &mut iface_decls, tokens)?;
            let has_implementation =
                parse_decls_section(Keyword::Implementation, &mut impl_decls, tokens)?;

            let init_kw = tokens.match_one_maybe(Keyword::Initialization);
            if let Some(init_kw) = &init_kw {
                let init_body = parse_init_section(tokens)?;
                
                init = Some(InitBlock {
                    keyword_span: init_kw.span().clone(),
                    body: init_body,
                })
            }

            if !(has_interface || has_implementation || init_kw.is_some()) {
                // empty units are invalid! use this to throw an error
                tokens.match_one(unit_kind_kw_match
                    .or(Keyword::Interface)
                    .or(Keyword::Implementation)
                    .or(Keyword::Initialization))?;
            }
        }

        tokens.match_one(Keyword::End)?;

        // allow the traditional period after the final end
        tokens.match_one_maybe(Operator::Period);

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

        Some(tt) if tt.is_keyword(Keyword::Const) || tt.is_keyword(Keyword::Var) => UnitDecl::GlobalBinding {
            decl: GlobalBinding::parse(tokens)?,
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
    let func_decl = Rc::new(FunctionDecl::parse(tokens)?);

    let body_ahead = if part_kw == Keyword::Interface {
        // interface funcs - never expect a body, unless the function is marked `inline`
        func_decl.mods.iter().any(|decl_mod| match decl_mod {
            DeclMod::Inline(..) => true,
            _ => false,
        })
    } else {
        // implementation funcs - always expect a body, unless the function has the
        // `external` (body is external) or `forward` (body to follow later) modifiers
        !func_decl.mods.iter().any(|decl_mod| match decl_mod {
            DeclMod::External { .. } | DeclMod::Forward(..) => true,
            _ => false,
        })
    };

    if body_ahead {
        tokens.match_one(Separator::Semicolon)?;

        let def = FunctionDef::parse_body_of_decl(func_decl, tokens)?;

        Ok(UnitDecl::FunctionDef { def: Rc::new(def) })
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
        
        if let Some(init_block) = &self.init {
            writeln!(f, "initialization")?;
            
            for init_stmt in &init_block.body {
                writeln!(f, "\t{};", init_stmt)?;
            }
        }

        writeln!(f, "end.")?;

        Ok(())
    }
}
