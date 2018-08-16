use std::fmt;

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Identifier {
    namespace: Vec<String>,
    name: String,
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.namespace.len() > 0 {
            write!(f, "{}.", self.namespace.join("."))?;
        }
        write!(f, "{}", self.name)
    }
}

impl Identifier {
    pub fn parse(source: &str) -> Self {
        let mut parts : Vec<String> = source.split('.')
            .map(|part: &str| part.to_owned())
            .collect();

        let name = parts.pop().unwrap_or(String::new());

        Self { namespace: parts, name }
    }
}
