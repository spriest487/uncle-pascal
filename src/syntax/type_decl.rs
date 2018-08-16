use types;

#[derive(Clone, Debug)]
pub struct RecordDecl {
    name: types::Identifier,
    members: Vec<RecordDecl>,
}
