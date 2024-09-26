use crate::ast::type_name::TypeName;
use crate::ast::{Ident, TypeParam};
use crate::parse::Match;
use crate::parse::Parse;
use crate::parse::ParseError;
use crate::parse::ParseResult;
use crate::parse::TokenStream;
use crate::DelimiterPair;
use crate::Separator;
use crate::TokenTree;
use derivative::Derivative;
use common::span::Span;
use common::span::Spanned;
use common::TracedError;
use std::fmt;
use std::ops::Index;
use std::ops::IndexMut;

/// Delimited list of types used for declaring and using generics
/// e.g. the part in square brackets of the following:
///
/// * `let y := MakeANewBox[Integer](123);`
/// * `let x: Box[Integer] := y;`
///
/// Generic because items may be type names (when they refer to real types in expressions)
/// or idents only (when they are declaring type parameter names in type/function declarations)
#[derive(Clone, Eq, Derivative)]
#[derivative(PartialEq, Hash, Debug)]
pub struct TypeList<Item> {
    pub items: Vec<Item>,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    span: Span,
}

impl<Item> TypeList<Item> {
    pub fn len(&self) -> usize {
        self.items.len()
    }

    pub fn new(items: impl IntoIterator<Item = Item>, span: Span) -> Self {
        let items: Vec<_> = items.into_iter().collect();
        if items.len() == 0 {
            panic!("can't construct an empty type list (@ {})", span);
        }

        Self { items, span }
    }

    pub fn iter(&self) -> impl Iterator<Item = &Item> {
        self.items.iter()
    }
}

impl<Item> Spanned for TypeList<Item> {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl<Item> fmt::Display for TypeList<Item>
where
    Item: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[")?;

        for (i, item) in self.items.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", item)?;
        }

        write!(f, "]")?;
        Ok(())
    }
}

impl<Item> Index<usize> for TypeList<Item> {
    type Output = Item;

    fn index(&self, index: usize) -> &Self::Output {
        &self.items[index]
    }
}

impl<Item> IndexMut<usize> for TypeList<Item> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.items[index]
    }
}

impl TypeList<TypeName> {
    pub(crate) fn parse_type_args(tokens: &mut TokenStream) -> ParseResult<Self> {
        let type_args = Self::parse(tokens)?;

        if type_args.items.len() == 0 {
            let err = ParseError::EmptyTypeArgList(type_args);
            return Err(TracedError::trace(err));
        }

        Ok(type_args)
    }
}

impl TypeList<TypeParam<TypeName>> {
    // parses a list of TypeParams with no constraints. constraints are specified later in a where
    // clause, so if the list should have constraints, those will need to be parsed separately
    // and applied to the results
    pub(crate) fn parse_type_params(tokens: &mut TokenStream) -> ParseResult<Self> {
        let raw_params = TypeList::<Ident>::parse(tokens)?;

        if raw_params.items.len() == 0 {
            let err = ParseError::EmptyTypeParamList(raw_params);
            return Err(TracedError::trace(err));
        }

        Ok(TypeList {
            items: raw_params
                .items
                .into_iter()
                .map(|param_ident| {
                    TypeParam {
                        name: param_ident,
                        constraint: None,
                    }
                })
                .collect(),
            span: raw_params.span,
        })
    }
}

impl<Item> TypeList<Item>
where
    Item: Parse + Match,
{
    // parse the type list inside the square brackets, for usages where we already consumed the
    // outer group
    pub(crate) fn parse_items(tokens: &mut TokenStream) -> ParseResult<Vec<Item>> {
        // empty type lists aren't valid, but we parse them first then check that later
        // so accept them for now
        let mut items = Vec::new();
        loop {
            let mut tokens_ahead = tokens.look_ahead();
            if !items.is_empty() && tokens_ahead.match_one(Separator::Comma).is_none() {
                break;
            }

            if !Item::is_match(&mut tokens_ahead) {
                break;
            }

            if !items.is_empty() {
                tokens.match_one(Separator::Comma)?;
            }

            let item = Item::parse(tokens)?;
            items.push(item);
        }
        
        Ok(items)
    }
}

impl<Item> Parse for TypeList<Item>
where
    Item: Spanned + Parse + Match
{
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let (span, mut items_tokens) = match tokens.match_one(DelimiterPair::SquareBracket)? {
            TokenTree::Delimited(group) => (group.span.clone(), group.to_inner_tokens()),
            _ => unreachable!(),
        };

        let items = Self::parse_items(&mut items_tokens)?;

        // allow redundant comma after final item
        items_tokens.match_one_maybe(Separator::Comma);
        items_tokens.finish()?;

        Ok(TypeList { items, span })
    }
}
