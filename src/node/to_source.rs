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

impl<C> ToSource for Block<C>
    where C: Context
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

impl<C> ToSource for Expression<C>
    where C: Context
{
    fn to_source(&self) -> String {
        self.value.to_source()
    }
}

impl<C> ToSource for ExpressionValue<C>
    where C: Context
{
    fn to_source(&self) -> String {
        match self {
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

            ExpressionValue::ArrayElement { of, index_expr } => {
                format!("{}[{}]", of.to_source(), index_expr.to_source())
            }

            ExpressionValue::LetBinding { name, value } => {
                format!("let {} := {}", name, value.to_source())
            }

            ExpressionValue::Constant(ConstantExpression::Integer(i)) => format!("{}", i),
            ExpressionValue::Constant(ConstantExpression::Float(f)) => format!("{}", f),

            ExpressionValue::Constant(ConstantExpression::String(s)) =>
                format!("{}", tokens::LiteralString(s.clone()).to_source()),

            ExpressionValue::Constant(ConstantExpression::Boolean(b)) =>
                format!("{}", if *b { "true" } else { "false" }),

            ExpressionValue::Constant(ConstantExpression::Nil) => "nil".to_string(),

            ExpressionValue::Constant(ConstantExpression::Enum(e)) =>
                format!("{}", e.name),

            ExpressionValue::Constant(ConstantExpression::Set(set)) =>
                format!("{}", set.set),

            ExpressionValue::If { condition, then_branch, else_branch } => {
                let mut lines = Vec::new();
                lines.push(format!("if {} then", condition.to_source()));
                lines.push(format!("\t{}", then_branch.to_source()));

                if let &Some(ref else_expr) = else_branch {
                    lines.push(format!("else\n\t{}", else_expr.to_source()))
                }

                lines.join("\n")
            }

            ExpressionValue::While { condition, body } => {
                format!("while {} do {}", condition.to_source(), body.to_source())
            }

            ExpressionValue::Block(block) => {
                block.to_source()
            }

            ExpressionValue::ForLoop { from, to, body } => {
                format!("for {} to {} do {}", from.to_source(), to.to_source(), body.to_source())
            }

            ExpressionValue::SetConstructor(members) => {
                format!("[{}]", members.iter()
                    .map(|member| {
                        let member_src = member.from.to_source();
                        match member.to.as_ref() {
                            Some(to) => format!("{}..{}", member_src, to.to_source()),
                            None => member_src,
                        }
                    })
                    .collect::<Vec<_>>()
                    .join(", "))
            }

            ExpressionValue::With { value, body } => {
                format!("with {} do {}", value.to_source(), body.to_source())
            }

            ExpressionValue::Raise(error) => {
                format!("raise {}", error.to_source())
            }
        }
    }
}

impl<C> ToSource for VarDecls<C>
    where C: Context
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

impl<C> ToSource for ConstDecls<C>
    where C: Context
{
    fn to_source(&self) -> String {
        let decl_lines = self.decls.iter()
            .map(|decl| {
                format!("\t{} = {};", decl.name, decl.value.to_source())
            })
            .collect::<Vec<_>>()
            .join("\n");

        format!("const\n{}", decl_lines)
    }
}

impl ToSource for ConstantExpression {
    fn to_source(&self) -> String {
        match self {
            ConstantExpression::Integer(int) => int.to_string(),
            ConstantExpression::Float(float) => float.to_string(),
            ConstantExpression::Nil => "nil".to_string(),
            ConstantExpression::Boolean(val) => match val {
                true => "true".to_string(),
                false => "false".to_string(),
            }
            ConstantExpression::Enum(enum_val) => enum_val.to_string(),
            ConstantExpression::Set(set_val) => set_val.to_string(),
            ConstantExpression::String(s) => format!("'{}'", s),
        }
    }
}

impl<C> ToSource for FunctionLocalDecl<C>
    where C: Context
{
    fn to_source(&self) -> String {
        match self {
            FunctionLocalDecl::Vars(vars) => vars.to_source(),
            FunctionLocalDecl::NestedFunction(func) => func.to_source(),
            FunctionLocalDecl::Consts(consts) => consts.to_source(),
        }
    }
}


impl<C> ToSource for Function<C>
    where C: Context
{
    fn to_source(&self) -> String {
        let mut out = Vec::new();
        out.push(self.decl.to_source());
        for decl in self.local_decls.iter() {
            out.push(decl.to_source());
        }
        out.push(self.block.to_source() + ";");

        out.join("\n")
    }
}

impl<C> ToSource for FunctionDecl<C>
    where C: Context
{
    // todo: this is incomplete
    fn to_source(&self) -> String {
        format!("function {};", self.name)
    }
}

impl<C> ToSource for Program<C>
    where C: Context
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
                Implementation::Function(func) =>
                    lines.push(func.to_source()),

                Implementation::Decl(UnitDecl::Type(type_decl)) =>
                    lines.push(type_decl.to_source()),

                Implementation::Decl(UnitDecl::Function(func_decl)) =>
                    lines.push(func_decl.to_source()),

                Implementation::Decl(UnitDecl::Vars(var_decls)) =>
                    lines.push(var_decls.to_source()),

                Implementation::Decl(UnitDecl::Consts(const_decls)) =>
                    lines.push(const_decls.to_source()),
            }
        }

        lines.push(self.program_block.to_source() + ".");

        lines.join("\n\n")
    }
}

impl<C> ToSource for TypeDecl<C>
    where C: Context
{
    fn to_source(&self) -> String {
        match self {
            TypeDecl::Alias { alias, of, .. } => {
                format!("type {} = {};", alias, of.to_source())
            }

            TypeDecl::Enumeration(enum_decl) => enum_decl.to_source(),
            TypeDecl::Set(set_decl) => set_decl.to_source(),

            TypeDecl::Record(record_decl) => record_decl.to_source()
        }
    }
}

impl<C> ToSource for RecordDecl<C>
    where C: Context
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

impl<C> ToSource for EnumerationDecl<C>
    where C: Context
{
    fn to_source(&self) -> String {
        format!("type {} = ({})", self.name, self.names.join(", "))
    }
}

impl<C> ToSource for SetDecl<C>
    where C: Context
{
    fn to_source(&self) -> String {
        format!("type {} = set of {}", self.name, match &self.enumeration {
            SetEnumeration::Named(enum_name) => enum_name.to_string(),
            SetEnumeration::Inline(names) => format!("({})", names.join(", ")),
        })
    }
}

impl<C> ToSource for UnitDecl<C>
    where C: Context
{
    fn to_source(&self) -> String {
        match self {
            UnitDecl::Type(type_decl) =>
                type_decl.to_source(),

            UnitDecl::Function(func_decl) =>
                func_decl.to_source(),

            UnitDecl::Vars(var_decls) =>
                var_decls.to_source(),

            UnitDecl::Consts(const_decls) =>
                const_decls.to_source(),
        }
    }
}

impl<C> ToSource for Unit<C>
    where C: Context
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