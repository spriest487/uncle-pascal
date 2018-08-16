use std::fmt::{self, Write};

use semantic;
use operators;
use node;
use types;

pub fn type_to_c(pascal_type: &types::DeclaredType) -> String {
    match pascal_type {
        &types::DeclaredType::None => "void".to_owned(),
        &types::DeclaredType::Integer => write_identifier(&types::builtin_names::system_integer()),
        &types::DeclaredType::String => write_identifier(&types::builtin_names::system_string()),
        &types::DeclaredType::Boolean => write_identifier(&types::builtin_names::system_boolean()),
        &types::DeclaredType::Pointer => write_identifier(&types::builtin_names::system_pointer()),
        &types::DeclaredType::Function(ref sig) => {
            let name = sig.name.clone(); //TODO: should be identifier
            let return_type = type_to_c(&sig.return_type);
            let arg_types = sig.arg_types.iter().map(type_to_c)
                .collect::<Vec<_>>()
                .join(", ");

            format!("({} (*{})({}))", return_type, name, arg_types)
        },
        &types::DeclaredType::Record(ref decl) => decl.name.clone(), //TODO: should be identifier
    }.to_owned()
}

pub fn write_identifier(id: &node::Identifier) -> String {
    let mut parts = id.namespace.clone();
    parts.push(id.name.clone());

    parts.join("_")
}

pub fn write_expr(out: &mut String, expr: &semantic::Expression) -> fmt::Result {
    match expr {
        &node::Expression::BinaryOperator { ref lhs, ref op, ref rhs } => {
            let c_op = match op {
                &operators::Assignment => "=",
                &operators::Equals => "==",
                &operators::Minus => "-",
                &operators::Plus => "+",
            };

            write!(out, "(")?;
            write_expr(out, lhs)?;
            write!(out, " {} ", c_op)?;
            write_expr(out, rhs)?;
            write!(out, ")")
        }

        &node::Expression::FunctionCall { ref target, ref args } => {
            let args_str = args.iter()
                .map(|arg_expr| -> Result<String, fmt::Error> {
                    let mut expr_out = String::new();
                    write_expr(&mut expr_out, arg_expr)?;

                    Ok(expr_out)
                })
                .collect::<Result<Vec<_>, _>>()?
                .join(", ");

            let target_name = &target.name;//TODO should be an identifier
            write!(out, "{}({})", target_name, args_str)
        }

        &node::Expression::LiteralInteger(ref i) => {
            write!(out, "(({}) {})", type_to_c(&types::DeclaredType::Integer), i)
        }

        &node::Expression::LiteralString(ref s) => {
            write!(out, "(({})\"{}\")", type_to_c(&types::DeclaredType::String), s)
        }

        &node::Expression::If { ref condition, ref then_branch, ref else_branch } => {
            write!(out, "if (")?;
            write_expr(out, condition)?;
            write!(out, ") ")?;

            write_expr(out, then_branch)?;

            if let &Some(ref else_expr) = else_branch {
                write!(out, " else ")?;
                write_expr(out, else_expr)?;
            }
            Ok(())
        }

        &node::Expression::Identifier(ref id) => {
            write!(out, "{}", write_identifier(&id.name))
        }

        &node::Expression::Block(ref block) => {
            write_block(out, block)
        }
    }
}

pub fn write_block(out: &mut String, block: &semantic::Block) -> fmt::Result {
    writeln!(out, "{{")?;

    for statement in block.statements.iter() {
        write_expr(out, statement)?;
        writeln!(out, ";")?;
    }

    writeln!(out, "}}")
}

pub fn write_vars(out: &mut String, vars: &semantic::Vars) -> fmt::Result {
    for decl in vars.decls.iter() {
        writeln!(out, "{} {};", type_to_c(&decl.decl_type), decl.name)?;
    }

    Ok(())
}

pub fn write_function(out: &mut String, function: &semantic::Function) -> fmt::Result {
    let return_type = type_to_c(&function.return_type);

    let args = function.args.decls.iter()
        .map(|arg_decl| {
            format!("{} {}", type_to_c(&arg_decl.decl_type), &arg_decl.name)
        })
        .collect::<Vec<_>>()
        .join(", ");

    writeln!(out, "{} {}({}) {{", return_type, function.name, args)?;

    for local_var in function.local_vars.decls.iter() {
        writeln!(out, "{} {};", type_to_c(&local_var.decl_type), local_var.name)?;
    }

    write_block(out, &function.body)?;
    writeln!(out, "}}")
}

pub fn write_c(program: &semantic::Program) -> Result<String, fmt::Error> {
    let mut output = String::new();

    writeln!(output, "#include <stdint.h>")?;
    writeln!(output, "#include <stdlib.h>")?;
    writeln!(output, "#include <stdio.h>")?;
    writeln!(output, "#include <string.h>")?;

    writeln!(output, "typedef const char* System_String;")?;
    writeln!(output, "typedef int64_t System_Integer;")?;
    writeln!(output, "typedef void* System_Pointer;")?;

    writeln!(output,
r"static void WriteLn(System_String ln) {{
    if (!ln) abort();

    puts(ln);
}}

static System_Pointer GetMem(System_Integer bytes) {{
    if (bytes > 0) {{
        return malloc((size_t) bytes);
    }} else {{
        return NULL;
    }}
}}

static void Dispose(System_Pointer p) {{
    free(p);
}}")?;

    for func in program.functions.iter() {
        write_function(&mut output, func)?;
    }

    write_vars(&mut output, &program.vars)?;

    writeln!(output, "int main(int argc, char* argv[]) {{")?;
    write_block(&mut output, &program.program_block)?;
    writeln!(output, "  return 0;")?;
    writeln!(output, "}}")?;

    Ok(output)
}