use syntax;
use types;
use operators;

//pub fn type_to_c(pascal_type: &types::Type) -> String {
//    match pascal_type {
//        &types::Type::Integer => "int64_t",
//        &types::Type::String => "const char*",
//    }.to_owned()
//}

pub fn write_identifier(id: &types::Identifier) -> String {
    let mut parts = id.namespace.clone();
    parts.push(id.name.clone());

    parts.join("_")
}

pub fn write_expr(expr: &syntax::Expression) -> String {
    match expr {
        &syntax::Expression::BinaryOperator { ref lhs, ref op, ref rhs } => {
            let c_op = match op {
                &operators::Assignment => "=",
                &operators::Equals => "==",
                &operators::Minus => "-",
                &operators::Plus => "+",
            };

            format!("({} {} {})", write_expr(lhs), c_op, write_expr(rhs))
        }

        &syntax::Expression::FunctionCall { ref target, ref args } => {
            let args_str = args.iter().map(|arg_expr| write_expr(arg_expr))
                .collect::<Vec<_>>()
                .join(", ");

            format!("{}({})", write_identifier(target), args_str)
        }

        &syntax::Expression::LiteralInteger(ref i) => {
            format!("((int64_t) {})", i)
        }

        &syntax::Expression::LiteralString(ref s) => {
            format!("((const char*)\"{}\")", s)
        }

        &syntax::Expression::If { ref condition, ref then_branch, ref else_branch } => {
            let then_branch = format!("if ({}) {}", write_expr(condition), write_expr(then_branch));

            match else_branch {
                &Some(ref else_expr) => format!("{} else {}", then_branch, write_expr(else_expr)),
                &None => then_branch
            }
        }

        &syntax::Expression::Identifier(ref id) => {
            write_identifier(id)
        }

        &syntax::Expression::Block(ref block) => {
            write_block(block)
        }
    }
}

pub fn write_block(block: &syntax::Block) -> String {
    let mut lines = Vec::new();
    lines.push("{".to_owned());

    for statement in block.statements.iter() {
        lines.push(format!("{};", write_expr(statement)));
    }

    lines.push("}".to_owned());

    lines.join("\n")
}

pub fn write_vars(vars: &syntax::Vars) -> String {
    let mut lines = Vec::new();
    for decl in vars.decls.iter() {
        lines.push(format!("{} {};", write_identifier(&decl.decl_type), decl.name));
    }

    lines.join("\n")
}

pub fn write_c(program: &syntax::Program) -> String {
    let writeln = r"
void WriteLn(const char* ln) {
    if (!ln) abort();

    puts(ln);
}
";

    let mut lines = Vec::new();
    lines.push("#include <stdint.h>".to_owned());
    lines.push("#include <stdlib.h>".to_owned());
    lines.push("#include <stdio.h>".to_owned());
    lines.push("#include <string.h>".to_owned());

    lines.push("typedef const char* String;".to_owned());
    lines.push("typedef int64_t Integer;".to_owned());

    lines.push(writeln.to_owned());

    lines.push(write_vars(&program.vars));

    lines.push(format!("int main(int argc, char* argv[]) {{ {}\nreturn 0;\n}}",
                       write_block(&program.program_block)));

    lines.join("\n")
}