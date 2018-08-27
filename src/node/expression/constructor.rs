use std::fmt;
use node::{
    Context,
    Expression,
};

#[derive(Clone, Debug)]
pub struct ObjectConstructorMember<TContext>
    where TContext: Context
{
    pub name: String,
    pub value: Expression<TContext>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ObjectConstructor<TContext>
    where TContext: Context
{
    pub members: Vec<ObjectConstructorMember<TContext>>,
    pub object_type: Option<TContext::Type>,
}

impl<TContext> ObjectConstructor<TContext>
    where TContext: Context
{
    pub fn get_member(&self, name: &str) -> Option<&ObjectConstructorMember<TContext>> {
        self.members.iter()
            .find(|mem| mem.name == name)
    }
}

impl<TContext> PartialEq for ObjectConstructorMember<TContext>
    where TContext: Context
{
    fn eq(&self, other: &Self) -> bool {
        self.name.eq(&other.name) && self.value.eq(&other.value)
    }
}

impl<C> fmt::Display for ObjectConstructor<C>
    where C: Context
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(")?;
        for (member_num, member) in self.members.iter().enumerate() {
            write!(f, "{}: {}", member.name, member.value)?;

            if member_num < self.members.len() - 1 {
                write!(f, ";")?;
            }
        }
        write!(f, ")")
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum CollectionMember<TContext>
    where TContext: Context
{
    Single(Expression<TContext>),
    Range {
        from: Expression<TContext>,
        to: Expression<TContext>,
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct CollectionConstructor<TContext>
    where TContext: Context
{
    pub element_type: Option<TContext::Type>,
    pub members: Vec<CollectionMember<TContext>>,
}

impl<TContext> fmt::Display for CollectionConstructor<TContext>
    where TContext: Context
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[")?;
        for (i, member) in self.members.iter().enumerate() {
            if i > 0 {
                f.write_str(", ")?;
            }

            match member {
                CollectionMember::Range { from, to } => {
                    write!(f, "{}..{}", from, to)?;
                }

                CollectionMember::Single(val) => {
                    write!(f, "{}", val)?;
                },
            }
        }
        write!(f, "]")
    }
}

impl<TContext> CollectionConstructor<TContext>
    where TContext: Context
{
    pub fn contains_ranges(&self) -> bool {
        self.members.iter().any(|element| match element {
            CollectionMember::Range { .. } => true,
            _ => false,
        })
    }

    /* iterate over the members as a flat array. panics if any of the member expressions
is a range */
    pub fn as_array(&self) -> impl Iterator<Item=&Expression<TContext>> {
        self.members.iter()
            .map(move |member| match member {
                CollectionMember::Single(expr) => expr,
                CollectionMember::Range { .. } => {
                    panic!("called array_values on `{}`, which contained ranges", self);
                },
            })
    }
}
