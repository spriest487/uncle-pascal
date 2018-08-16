use tokens;
use node::*;

pub trait ToSource {
    fn to_source(&self) -> String;
}

impl ToSource for Identifier {
    fn to_source(&self) -> String {
        self.to_string()
    }
}

impl<T, C> ToSource for Block<T, C>
    where T: Symbol,
          C: Context
{
    fn to_source(&self) -> String {
        let mut lines = Vec::new();
        lines.push("begin".to_owned());

        for (i, statement) in self.statements.iter().enumerate() {
            let mut line = statement.to_source();
            if i < self.statements.len() - 1 {
                line = line + ";";
            }

            lines.push(format!("\t{}", line));
        }

        lines.push("end".to_string()).to_owned();
        lines.join("\n")
    }
}

impl<T, C> ToSource for Expression<T, C>
    where T: Symbol,
          C: Context
{
    fn to_source(&self) -> String {
        match &self.value {
            ExpressionValue::BinaryOperator { lhs, op, rhs } => {
                format!("({} {} {})", lhs.to_source(), op, rhs.to_source())
            }

            ExpressionValue::Identifier(id) => id.to_source(),

            ExpressionValue::FunctionCall { target, args } => {
                let args_str = args.iter()
                    .map(|arg| arg.to_source())
                    .collect::<Vec<_>>()
                    .join(", ");

                format!("{}({})", target.to_source(), args_str)
            }

            ExpressionValue::PrefixOperator { op, rhs } => {
                format!("({} {})", op, rhs.to_source())
            }

            ExpressionValue::Member { of, name } => {
                format!("{}.{}", of.to_source(), name)
            }

            ExpressionValue::LetBinding { name, value } => {
                format!("let {} := {}", name, value.to_source())
            }

            ExpressionValue::LiteralInteger(i) => format!("{}", i),

            ExpressionValue::LiteralString(s) =>
                format!("'{}'", tokens::LiteralString(s.clone()).to_source()),

            ExpressionValue::LiteralNil => "nil".to_string(),

            ExpressionValue::If { condition, then_branch, else_branch } => {
                let mut lines = Vec::new();
                lines.push(format!("if {} then", condition.to_source()));
                lines.push(format!("\t{}", then_branch.to_source()));

                if let &Some(ref else_expr) = else_branch {
                    lines.push(format!("else\n\t{}", else_expr.to_source()))
                }

                lines.join("\n")
            }

            ExpressionValue::Block(block) => {
                block.to_source()
            }

            ExpressionValue::ForLoop { from, to, body } => {
                format!("for {} to {} do {}", from.to_source(), to.to_source(), body.to_source())
            }
        }
    }
}

impl<T, C> ToSource for VarDecls<T, C>
    where T: Symbol,
          C: Context
{
    fn to_source(&self) -> String {
        let decl_lines = self.decls.iter()
            .map(|decl| {
                format!("\t{}: {};", decl.name, decl.decl_type.to_source())
            })
            .collect::<Vec<_>>()
            .join("\n");

        format!("var\n{}", decl_lines)
    }
}

impl<T, C> ToSource for FunctionDecl<T, C>
    where T: Symbol,
          C: Context
{
    fn to_source(&self) -> String {
        let mut lines = Vec::new();
        lines.push(format!("function {};", self.name));

        if let Some(body) = &self.body {
            if body.local_vars.decls.len() > 0 {
                lines.push(body.local_vars.to_source());
            }

            lines.push(body.block.to_source() + ";");
        }

        lines.join("\n")
    }
}

impl<T, C> ToSource for Program<T, C>
    where T: Symbol,
          C: Context
{
    fn to_source(&self) -> String {
        let mut lines = Vec::new();
        lines.push(format!("program {};", self.name));

        if self.uses.len() > 0 {
            lines.push(format!("uses {};",
                               self.uses.iter().map(|u| format!("{}", u))
                                   .collect::<Vec<_>>()
                                   .join(", ")));
        }

        for decl in self.decls.iter() {
            match decl {
                UnitDeclaration::Type(type_decl) =>
                    lines.push(type_decl.to_source()),

                UnitDeclaration::Function(func_decl) =>
                    lines.push(func_decl.to_source()),

                UnitDeclaration::Vars(var_decls) =>
                    lines.push(var_decls.to_source()),
            }
        }

        lines.push(self.program_block.to_source() + ".");

        lines.join("\n\n")
    }
}

impl<T, C> ToSource for TypeDecl<T, C>
    where T: Symbol,
          C: Context
{
    fn to_source(&self) -> String {
        match self {
            TypeDecl::Alias { alias, of, .. } => {
                format!("type {} = {};", alias, of.to_source())
            }
            TypeDecl::Record(record_decl) => record_decl.to_source()
        }
    }
}

impl<T, C> ToSource for RecordDecl<T, C>
    where T: Symbol,
          C: Context
{
    fn to_source(&self) -> String {
        let mut lines = Vec::new();
        lines.push(format!("type {} = record", self.name));

        for member in self.members.iter() {
            lines.push(format!("\t{}: {};", member.name, member.decl_type.to_source()));
        }

        lines.push("end".to_owned());
        lines.join("\n")
    }
}

impl<T, C> ToSource for UnitDeclaration<T, C>
    where T: Symbol,
          C: Context
{
    fn to_source(&self) -> String {
        match self {
            UnitDeclaration::Type(type_decl) =>
                type_decl.to_source(),

            UnitDeclaration::Function(func_decl) =>
                func_decl.to_source(),

            UnitDeclaration::Vars(var_decls) =>
                var_decls.to_source(),
        }
    }
}

impl<T, C> ToSource for Unit<T, C>
    where T: Symbol,
          C: Context
{
    fn to_source(&self) -> String {
        let mut lines = Vec::new();
        lines.push(format!("unit {};", self.name));

        if self.uses.len() > 0 {
            lines.push(format!("uses {};",
                               self.uses.iter().map(|u| format!("{}", u))
                                   .collect::<Vec<_>>()
                                   .join(", ")));
        }

        lines.push("interface".to_owned());
        for decl in self.interface.iter() {
            lines.push(decl.to_source())
        }

        lines.push("implementation".to_owned());
        for decl in self.interface.iter() {
            lines.push(decl.to_source())
        }

        lines.join("\n\n")
    }
}