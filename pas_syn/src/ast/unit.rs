use {
    std::{
        fmt,
    },
    crate::{
        parse::*,
        token_tree::*,
        span::*,
        keyword::*,
        ast::*,
    }
};

#[derive(Clone, Debug)]
pub enum UnitDecl<A: Annotation> {
    Function(FunctionDecl<A>),
}

impl<A: Annotation> Spanned for UnitDecl<A> {
    fn span(&self) -> &Span {
        match self {
            UnitDecl::Function(func_decl) => func_decl.span(),
        }
    }
}

impl<A: Annotation> fmt::Display for UnitDecl<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            UnitDecl::Function(func) => write!(f, "{}", func),
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
        let match_decl_kw = Matcher::from(Keyword::Function)
            .or(Keyword::Type);

        let decls = tokens.match_separated(Separator::Semicolon, |_, tokens| {
            match tokens.look_ahead().match_one(match_decl_kw.clone()) {
                Some(TokenTree::Keyword { kw: Keyword::Function, .. }) => {
                    let func_decl = FunctionDecl::parse(tokens)?;

                    Ok(Generate::Yield(UnitDecl::Function(func_decl)))
                }

                Some(_) | None => Ok(Generate::Break),
            }
        })?;

        let statements = tokens.match_separated(Separator::Semicolon, |_i, tokens: &mut TokenStream| {
            if tokens.look_ahead().match_one(statement_start_matcher()).is_none() {
                return Ok(Generate::Break);
            }

            let stmt = Statement::parse(tokens)?;
            Ok(Generate::Yield(stmt))
        })?;

        Ok(Unit {
            init: statements,
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