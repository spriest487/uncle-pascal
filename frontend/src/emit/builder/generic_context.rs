use crate::typ;
use crate::typ::{Type, TypeArgList, TypeArgResolver, TypeParam, TypeParamContainer, TypeParamList};
use std::borrow::Cow;

#[derive(Debug, Clone)]
pub struct GenericContext {
    items: Vec<ResolvedTypeArg>,
}

impl GenericContext {
    pub fn new() -> Self {
        Self {
            items: Vec::new(),
        }
    }
    
    pub fn add_all(&mut self, params: &TypeParamList, args: &TypeArgList) {
        assert_eq!(params.len(), args.len());
        
        for (param, arg) in params.iter().zip(args.iter()) {
            self.add(param.clone(), arg.clone());
        }
    }
    
    pub fn append(&mut self, other: &mut Self) {
        self.items.append(&mut other.items);
    }
    
    pub fn add(&mut self, param: TypeParam, arg: Type) {
        self.items.push(ResolvedTypeArg {
            arg,
            param,
        })
    }
}

#[derive(Debug, Clone)]
struct ResolvedTypeArg {
    pub param: typ::TypeParam,
    pub arg: typ::Type,
}

impl TypeArgResolver for GenericContext {
    fn resolve(&self, param: &typ::TypeParamType) -> Cow<typ::Type> {
        let pos = self
            .find_position(param.name.name.as_str())
            .unwrap_or_else(|| panic!("missing type param {}", param.name));

        Cow::Borrowed(&self.items[pos].arg)
    }

    fn find_by_pos(&self, pos: usize) -> Option<&typ::Type> {
        self.items.get(pos).map(|item| &item.arg)
    }

    fn len(&self) -> usize {
        self.items.len()
    }
}

impl TypeParamContainer for GenericContext {
    fn find_position(&self, name: &str) -> Option<usize> {
        self.items
            .iter()
            .position(|p| p.param.name.name.as_str() == name)
    }

    fn len(&self) -> usize {
        self.items.len()
    }
}
