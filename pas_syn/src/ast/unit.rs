use {
    crate::{
        parse::*,
        token_tree::*,
        ast::*,
    },
    std::{
        fmt,
    },
    pas_common::{
        span::*,
    },
};

#[derive(Clone, Debug)]
pub enum UnitDecl<A: Annotation> {
    FunctionDecl(FunctionDecl<A>),
    FunctionDef(FunctionDef<A>),
    Type(TypeDecl<A>),
}

impl<A: Annotation> Spanned for UnitDecl<A> {
    fn span(&self) -> &Span {
        match self {
            UnitDecl::FunctionDecl(func_decl) => func_decl.span(),
            UnitDecl::FunctionDef(func_def) => func_def.span(),
            UnitDecl::Type(type_decl) => type_decl.span(),
        }
    }
}

impl<A: Annotation> fmt::Display for UnitDecl<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            UnitDecl::FunctionDecl(func_decl) => write!(f, "{}", func_decl),
            UnitDecl::FunctionDef(func_def) => write!(f, "{}", func_def),
            UnitDecl::Type(ty_decl) => write!(f, "{}", ty_decl),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Unit<A: Annotation> {
    pub decls: Vec<UnitDecl<A>>,
    pub init: Vec<Statement<A>>,
}

impl<A: Annotation> Unit<A> {
    pub fn func_decls(&self) -> impl Iterator<Item=&FunctionDecl<A>> {
        self.decls.iter()
            .filter_map(|decl| match decl {
                UnitDecl::FunctionDecl(func) => Some(func),
                UnitDecl::FunctionDef(func_def) => Some(&func_def.decl),
                _ => None,
            })
    }

    pub fn func_defs(&self) -> impl Iterator<Item=&FunctionDef<A>> {
        self.decls.iter()
            .filter_map(|decl| match decl {
                UnitDecl::FunctionDef(func_def) => Some(func_def),
                _ => None,
            })
    }

    pub fn type_decls(&self) -> impl Iterator<Item=&TypeDecl<A>> {
        self.decls.iter()
            .filter_map(|decl| match decl {
                UnitDecl::Type(ty) => Some(ty),
                _ => None,
            })
    }
}

impl Unit<Span> {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let mut decls = Vec::new();

        // can't use match_separated here because it doesn't play nicely
        // with the fact that type decls are also semicolon-separated lists
        loop {
            match tokens.look_ahead().next() {
                Some(TokenTree::Keyword { kw: Keyword::Function, .. }) => {
                    let func_decl = FunctionDecl::parse(tokens)?;
                    if tokens.look_ahead().match_one(DelimiterPair::BeginEnd).is_some() {
                        let body = Block::parse(tokens)?;

                        decls.push(UnitDecl::FunctionDef(FunctionDef {
                            span: func_decl.span().to(body.span()),
                            decl: func_decl,
                            body,
                        }))
                    } else {
                        decls.push(UnitDecl::FunctionDecl(func_decl));
                    }
                }

                Some(TokenTree::Keyword { kw: Keyword::Type, .. }) => {
                    let ty_decl = TypeDecl::parse(tokens)?;
                    decls.push(UnitDecl::Type(ty_decl));
                }

                _ => break,
            }

            match tokens.look_ahead().match_one(Separator::Semicolon) {
                Some(_) => tokens.advance(1),
                None => break,
            }
        }

        let init = tokens.match_separated(Separator::Semicolon, |_i, tokens: &mut TokenStream| {
            if tokens.look_ahead().match_one(statement_start_matcher()).is_none() {
                return Ok(Generate::Break);
            }

            let stmt = Statement::parse(tokens)?;
            Ok(Generate::Yield(stmt))
        })?;

        Ok(Unit {
            init,
            decls,
        })
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