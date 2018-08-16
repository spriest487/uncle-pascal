use syntax::Expression;

#[derive(Clone, Debug, PartialEq)]
pub struct IndexRange {
    pub from: Expression,
    pub to: Expression,
}
