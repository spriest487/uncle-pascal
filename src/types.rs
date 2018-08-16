#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Identifier {
    namespace: Vec<String>,
    name: String,
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