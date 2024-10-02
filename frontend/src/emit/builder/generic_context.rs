use crate::typ;
use crate::typ::Type;
use crate::typ::TypeArgList;
use crate::typ::TypeArgResolver;
use crate::typ::TypeParam;
use crate::typ::TypeParamContainer;
use crate::typ::TypeParamList;
use std::borrow::Cow;
use std::fmt;
use std::fmt::Formatter;

#[derive(Debug, Clone)]
pub struct GenericContext {
    items: Vec<ResolvedTypeArg>,
}

impl GenericContext {
    pub fn empty() -> Self {
        Self {
            items: Vec::new(),
        }
    }
    
    pub fn new(params: &TypeParamList, args: &TypeArgList) -> Self {
        assert_eq!(params.len(), args.len());
        
        let mut ctx = GenericContext::empty();
        
        for (param, arg) in params.iter().zip(args.iter()) {
            ctx.add(param.clone(), arg.clone());
        }
        
        ctx
    }
    
    pub fn child_context(&self, params: &TypeParamList, args: &TypeArgList) -> Self {
        assert_eq!(params.len(), args.len());

        let params = params.clone().apply_type_args_by_name(self, self);        
        let args = args.clone().apply_type_args_by_name(self, self);

        let mut child = self.clone();
        for (param, arg) in params.items.into_iter().zip(args.items.into_iter()) {
            child.add(param.clone(), arg.clone());
        }
        
        child
    }
    
    pub fn push(&mut self, params: &TypeParamList, args: &TypeArgList) {
        *self = self.child_context(params, args);
    }
    
    pub fn append(&mut self, other: &mut Self) {
        self.items.append(&mut other.items);
    }
    
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }
    
    fn add(&mut self, param: TypeParam, arg: Type) {
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

impl fmt::Display for GenericContext {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "[")?;
        
        for i in 0..self.items.len() {
            if i > 0 {
                write!(f, ", ")?;
            }
            
            write!(f, "{}", self.items[i].param.name)?;
            if let Some(constraint) = &self.items[i].param.constraint {
                write!(f, ": {}", constraint.is_ty)?;
            }
            
            write!(f, " = {}", self.items[i].arg)?;
        }
        
        write!(f, "]")
    }
}