use std::fmt;
use node::*;

#[derive(Clone, Debug, PartialEq)]
pub struct Block<TContext>
    where TContext: Context
{
    pub context: TContext,
    pub statements: Vec<Expression<TContext>>,
}

impl<TContext> Block<TContext>
    where TContext: Context
{
    pub fn write_statements(&self, out: &mut fmt::Write) -> fmt::Result {
        for (i, statement) in self.statements.iter().enumerate() {
            write!(out, "\t{}", statement)?;
            if i < self.statements.len() - 1 {
                writeln!(out, ";")?;
            } else {
                writeln!(out)?;
            }
        }

        Ok(())
    }
}

impl<TContext> fmt::Display for Block<TContext>
    where TContext: Context
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "begin")?;
        self.write_statements(f)?;
        writeln!(f, "end")
    }
}
