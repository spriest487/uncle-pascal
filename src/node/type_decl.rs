use std::{
    fmt,
    iter,
};
use linked_hash_map::LinkedHashMap;
use node::*;

#[derive(Clone, Debug)]
pub enum TypeDecl<TContext>
    where TContext: Context
{
    Record(RecordDecl<TContext>),
    Enumeration(EnumerationDecl<TContext>),
    Set(SetDecl<TContext>),
    Interface(InterfaceDecl<TContext>),
    Alias {
        context: TContext,

        alias: String,
        of: TContext::Type,
    },
}

impl<C> fmt::Display for TypeDecl<C>
    where C: Context
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeDecl::Alias { alias, of, .. } => {
                writeln!(f, "type {} = {};", alias, of)
            }

            TypeDecl::Enumeration(enum_decl) => write!(f, "{}", enum_decl),
            TypeDecl::Set(set_decl) => write!(f, "{}", set_decl),

            TypeDecl::Record(record_decl) => write!(f, "{}", record_decl),
            TypeDecl::Interface(interface_decl) => write!(f, "{}", interface_decl),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct InterfaceDecl<TContext>
    where TContext: Context
{
    pub name: String,
    pub context: TContext,
    pub methods: LinkedHashMap<String, FunctionSignature<TContext::Type>>,
}

impl<C> fmt::Display for InterfaceDecl<C>
    where C: Context
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "type {} = interface", self.name)?;

        for (name, sig) in self.methods.iter() {
            write!(f, "function {}(", name)?;
            for (i, arg) in sig.args.iter().enumerate() {
                write!(f, "{}", arg)?;
                if i < sig.args.len() {
                    f.write_str("; ")?;
                }
            }

            if let Some(return_type) = sig.return_type.as_ref() {
                write!(f, ": {}", return_type)?;
            }

            for modifier in sig.modifiers.iter() {
                write!(f, "; {}", modifier)?;
            }

            writeln!(f, ";")?;
        }

        writeln!(f, "end")
    }
}

#[derive(Clone, Debug)]
pub struct EnumerationDecl<TContext> {
    pub name: String,

    pub names: Vec<String>,
    pub context: TContext,
}

impl<C> fmt::Display for EnumerationDecl<C>
    where C: Context
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "type {} = ({});", self.name, self.names.join(", "))
    }
}

#[derive(Clone, Debug)]
pub struct SetDecl<TContext> {
    pub name: String,
    pub context: TContext,
    pub enumeration: SetEnumeration,
}

impl<C> fmt::Display for SetDecl<C>
    where C: Context
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "type {} = set of {};", self.name, match &self.enumeration {
            SetEnumeration::Named(enum_name) => enum_name.to_string(),
            SetEnumeration::Inline(names) => format!("({})", names.join(", ")),
        })
    }
}

#[derive(Clone, Debug)]
pub enum SetEnumeration {
    Named(Identifier),
    Inline(Vec<String>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct RecordVariantCase<TContext>
    where TContext: Context
{
    pub tag_value: Expression<TContext>,
    pub members: Vec<RecordMember<TContext>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct RecordMember<TContext>
    where TContext: Context
{
    pub name: String,
    pub decl_type: TContext::Type,
    pub context: TContext,
}

/*
variable structure/union
```
case tag: Int32 of
1: (foo: String; bar: Int32)
2: (baz: Boolean)
```
*/
#[derive(Clone, Debug, PartialEq)]
pub struct RecordVariantPart<TContext>
    where TContext: Context
{
    pub tag: RecordMember<TContext>,
    pub context: TContext,
    pub cases: Vec<RecordVariantCase<TContext>>,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum RecordKind {
    Record,
    Class,
}

#[derive(Clone, Debug)]
pub struct RecordDecl<TContext>
    where TContext: Context
{
    pub name: String,
    pub kind: RecordKind,

    pub context: TContext,
    pub members: Vec<RecordMember<TContext>>,
    pub variant_part: Option<RecordVariantPart<TContext>>,
}

impl<TContext> RecordDecl<TContext>
    where TContext: Context
{
    pub fn get_member(&self, name: &str) -> Option<&RecordMember<TContext>> {
        self.members.iter()
            .find(|member| member.name.as_str() == name)
            .or_else(|| {
                let variant_part = self.variant_part.as_ref()?;

                let tag = self.variant_part.as_ref().map(|variant_part| &variant_part.tag)?;
                match name == tag.name.as_str() {
                    true => Some(tag),
                    false => {
                        variant_part.cases.iter()
                            .flat_map(|case| case.members.iter())
                            .find(|member| member.name.as_str() == name)
                    }
                }
            })
    }

    /**
    iterate over all members, including possible variant tag and variant case members
*/
    pub fn all_members(&self) -> impl Iterator<Item=&RecordMember<TContext>> {
        self.members.iter()
            .chain(self.variant_part.iter()
                .flat_map(|variant_part| {
                    let tag = iter::once(&variant_part.tag);
                    let case_members = variant_part.cases.iter()
                        .flat_map(|case| {
                            case.members.iter()
                        });
                    tag.chain(case_members)
                }))
    }
}

impl<C> fmt::Display for RecordDecl<C>
    where C: Context
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "type {} = record", self.name)?;

        for member in self.members.iter() {
            writeln!(f, "\t{}: {};", member.name, member.decl_type)?;
        }

        writeln!(f, "end")
    }
}