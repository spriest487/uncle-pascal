use target_c::{
    ast::TranslationResult,
};
use semantic::{
    self,
    Scope,
};

pub struct Interface {}

impl Interface {
    pub fn translate(_decl: &semantic::InterfaceDecl,
                     _unit_scope: &Scope)
                     -> TranslationResult<Self> {
        Ok(Interface {})
    }
}