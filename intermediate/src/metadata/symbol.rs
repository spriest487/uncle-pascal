use std::fmt;
use ir_lang::Type;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Symbol {
    path: Vec<String>,
    type_args: Option<Vec<Type>>,
}

impl Symbol {
    pub fn new(name: impl Into<String>, ns: impl IntoIterator<Item=impl Into<String>>) -> Self {
        let mut path: Vec<String> = ns.into_iter().map(Into::into).collect();
        path.push(name.into());

        Self {
            path,
            type_args: None,
        }
    }

    pub fn path(&self) -> impl Iterator<Item=&String> {
        self.path.iter()
    }

    pub fn with_ty_args(self, args: Vec<Type>) -> Self {
        assert_eq!(
            None, self.type_args,
            "shouldn't already have type args when building a specialized GlobalName"
        );

        Self {
            type_args: Some(args),
            ..self
        }
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (i, part) in self.path.iter().enumerate() {
            if i > 0 {
                write!(f, "::")?;
            }
            write!(f, "{}", part)?;
        }

        if let Some(type_args) = self.type_args.as_ref() {
            write!(f, "<")?;
            for (i, arg) in type_args.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", arg)?;
            }
            write!(f, ">")?;
        }

        Ok(())
    }
}
