use std::fmt;
use node::*;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum UnitReferenceKind {
    /**
    adds all symbols from the unit to the scope,
    under their original namespace

    `uses System`
*/
    Namespaced,

    /**
    adds all symbols from the unit to the scope,
    `uses System.*`
*/
    All,

    /**
    `uses System.String` reference a particular name
*/
    Name(String),
}

#[derive(Clone, Debug)]
pub struct UnitReference<TContext>
    where TContext: Context
{
    pub name: Identifier,
    pub kind: UnitReferenceKind,
    pub context: TContext,
}

impl<TContext> fmt::Display for UnitReference<TContext>
    where TContext: Context
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let name = match &self.kind {
            UnitReferenceKind::Namespaced => self.name.to_string(),
            UnitReferenceKind::All => format!("{}.*", self.name),
            UnitReferenceKind::Name(name) => format!("{}.{}", self.name, name),
        };

        write!(f, "{} ({})", name, self.context.token())
    }
}

#[derive(Clone, Debug)]
pub enum UnitDecl<TContext>
    where TContext: Context
{
    Function(FunctionDecl<TContext>),
    Type(TypeDecl<TContext>),
    Var(VarDecl<TContext>),
    Const(ConstDecl<TContext>),
}

impl<TContext> UnitDecl<TContext>
    where TContext: Context
{
    pub fn as_class_decl(&self) -> Option<&RecordDecl<TContext>> {
        match self {
            UnitDecl::Type(TypeDecl::Record(record)) => {
                match record.kind {
                    RecordKind::Class => Some(record),
                    RecordKind::Record => None
                }
            }

            _ => None
        }
    }
}

impl<C> fmt::Display for UnitDecl<C>
    where C: Context
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            UnitDecl::Type(type_decl) => write!(f, "{}", type_decl),
            UnitDecl::Function(func_decl) => write!(f, "{}", func_decl),
            UnitDecl::Var(var) => write!(f, "{}", var),
            UnitDecl::Const(const_decl) => write!(f, "{}", const_decl),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Implementation<TContext>
    where TContext: Context
{
    Function(Function<TContext>),
    Decl(UnitDecl<TContext>),
}

impl<TContext> Implementation<TContext>
    where TContext: Context
{
    pub fn as_class_decl(&self) -> Option<&RecordDecl<TContext>> {
        match self {
            Implementation::Decl(decl) => decl.as_class_decl(),
            Implementation::Function(_) => None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Program<TContext>
    where TContext: Context
{
    pub name: String,

    pub uses: Vec<UnitReference<TContext>>,
    pub decls: Vec<Implementation<TContext>>,

    pub program_block: Block<TContext>,
}

impl<TContext> Program<TContext>
    where TContext: Context
{
    pub fn vars(&self) -> impl Iterator<Item=&VarDecl<TContext>> {
        self.decls.iter()
            .filter_map(|program_decl| match program_decl {
                Implementation::Decl(decl) => match decl {
                    UnitDecl::Var(var_decl) => Some(var_decl),
                    _ => None,
                }

                _ => None,
            })
    }
}

impl<C> fmt::Display for Program<C>
    where C: Context
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "program {};", self.name)?;

        if !self.uses.is_empty() {
            writeln!(f, "uses")?;

            for (i, unit_ref) in self.uses.iter().enumerate() {
                write!(f, "{}", unit_ref)?;
                if i == self.uses.len() - 1 {
                    writeln!(f, ";")?;
                } else {
                    f.write_str(", ")?;
                }
            }
        }

        for decl in &self.decls {
            match decl {
                Implementation::Function(func) =>
                    writeln!(f, "{}", func)?,

                Implementation::Decl(UnitDecl::Type(type_decl)) =>
                    writeln!(f, "{}", type_decl)?,

                Implementation::Decl(UnitDecl::Function(func_decl)) =>
                    writeln!(f, "{}", func_decl)?,

                Implementation::Decl(UnitDecl::Var(var_decl)) =>
                    writeln!(f, "{}", var_decl)?,

                Implementation::Decl(UnitDecl::Const(const_decl)) =>
                    writeln!(f, "{}", const_decl)?,
            }
        }

        writeln!(f, "{}.", self.program_block)
    }
}

#[derive(Clone, Debug)]
pub struct Unit<TContext>
    where TContext: Context
{
    pub name: String,

    pub uses: Vec<UnitReference<TContext>>,

    pub interface: Vec<UnitDecl<TContext>>,
    pub implementation: Vec<Implementation<TContext>>,

    pub initialization: Option<Block<TContext>>,
    pub finalization: Option<Block<TContext>>,
}

impl<TContext> Unit<TContext>
    where TContext: Context
{
    pub fn vars(&self) -> impl Iterator<Item=&VarDecl<TContext>> {
        let interface_vars = self.interface.iter()
            .filter_map(|decl| match decl {
                UnitDecl::Var(var) => Some(var),
                _ => None,
            });

        let impl_vars = self.implementation.iter()
            .filter_map(|program_decl| match program_decl {
                Implementation::Decl(decl) => match decl {
                    UnitDecl::Var(var) => Some(var),
                    _ => None,
                }

                _ => None,
            });

        interface_vars.chain(impl_vars)
    }
}

impl<C> fmt::Display for Unit<C>
    where C: Context
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "unit {};", self.name)?;

        if !self.uses.is_empty() {
            writeln!(f, "uses {};", self.uses.iter()
                .map(|u| format!("{}", u))
                .collect::<Vec<_>>()
                .join(", "))?;
        }

        writeln!(f, "interface")?;
        for decl in &self.interface {
            writeln!(f, "{}", decl)?;
        }

        writeln!(f, "implementation")?;
        for decl in &self.interface {
            writeln!(f, "{}", decl)?;
        }

        if let Some(block) = self.initialization.as_ref() {
            writeln!(f, "initialization")?;
            block.write_statements(f)?;
        }

        if let Some(block) = self.finalization.as_ref() {
            writeln!(f, "finalization")?;
            block.write_statements(f)?;
        }

        writeln!(f, "end.")
    }
}