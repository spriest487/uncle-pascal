use {
    std::{
        fmt,
    },
    pas_common::{
        span::*,
    },
    crate::{
        parse::*,
        token_tree::*,
        keyword::*,
        ast::*,
    }
};

#[derive(Clone, Debug)]
pub enum UnitDecl<A: Annotation> {
    Function(FunctionDecl<A>),
    Type(TypeDecl<A>),
}

impl<A: Annotation> Spanned for UnitDecl<A> {
    fn span(&self) -> &Span {
        match self {
            UnitDecl::Function(func_decl) => func_decl.span(),
            UnitDecl::Type(type_decl) => type_decl.span(),
        }
    }
}

impl<A: Annotation> fmt::Display for UnitDecl<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            UnitDecl::Function(func) => write!(f, "{}", func),
            UnitDecl::Type(ty_decl) => write!(f, "{}", ty_decl),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Unit<A: Annotation> {
    pub decls: Vec<UnitDecl<A>>,
    pub init: Vec<Statement<A>>,
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
                    decls.push(UnitDecl::Function(func_decl));

                    match tokens.look_ahead().match_one(Separator::Semicolon) {
                        Some(_) => tokens.advance(1),
                        None => break,
                    }
                }

                Some(TokenTree::Keyword { kw: Keyword::Type, .. }) => {
                    let ty_decls = TypeDecl::parse(tokens)?;
                    decls.extend(ty_decls.into_iter().map(UnitDecl::Type));
                }

                _ => break,
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