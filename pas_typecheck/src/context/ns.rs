use std::{borrow::Borrow, fmt, hash::Hash};
use pas_syn::{Ident, IdentPath};
use pas_syn::ast::Visibility;
use crate::{Decl, PathRef, Scope};

#[derive(Debug)]
pub struct AlreadyDeclared(pub Vec<Ident>, pub NameKind);

#[derive(Debug)]
pub struct NotDefinedHere(pub Ident);
