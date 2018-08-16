use std::fmt::{self, Write};

use semantic;
use operators;
use node;
use types;

pub fn type_to_c(pascal_type: &types::DeclaredType) -> String {
    match pascal_type {
        &types::DeclaredType::Byte => "System_Byte".to_owned(),
        &types::DeclaredType::Integer => "System_Integer".to_owned(),
        &types::DeclaredType::String => "System_String".to_owned(),
        &types::DeclaredType::Boolean => "System_Boolean".to_owned(),
        &types::DeclaredType::RawPointer => "System_Pointer".to_owned(),
        &types::DeclaredType::Pointer(ref target) => {
            let target_c = type_to_c(target.as_ref());
            format!("{}*", target_c)
        }
        &types::DeclaredType::Function(ref sig) => {
            let name = sig.name.clone(); //TODO: should be identifier
            let return_type = sig.return_type.as_ref()
                .map(type_to_c)
                .unwrap_or_else(|| "void".to_owned());
            let arg_types = sig.arg_types.iter()
                .map(type_to_c)
                .collect::<Vec<_>>()
                .join(", ");

            format!("({} (*{})({}))", return_type, name, arg_types)
        },
        &types::DeclaredType::Record(ref decl) => {
            //TODO: should be identifier
            format!("struct {}", decl.name)
        },
    }.to_owned()
}

pub fn identifier_to_c(id: &node::Identifier) -> String {
    let mut parts = id.namespace.clone();
    parts.push(id.name.clone());

    parts.join("_")
}

pub fn symbol_to_c(sym: &semantic::ScopedSymbol) -> String {
    match sym {
        &semantic::ScopedSymbol::Local { ref name, .. } => {
            name.to_owned()
        }

        &semantic::ScopedSymbol::Child { ref name, ref scope, .. } => {
            identifier_to_c(&scope.child(name))
        }

        &semantic::ScopedSymbol::RecordMember { ref record_id, ref name, .. } => {
            format!("{}.{}", identifier_to_c(record_id), name)
        }
    }
}

pub fn default_initialize(out: &mut String, target: &types::Symbol) -> fmt::Result {
    let id = identifier_to_c(&target.name);

    match &target.decl_type {
        &types::DeclaredType::Record(ref _record) => {
            writeln!(out, "memset(&{}, 0, sizeof({}));", id, id)
        }

        &types::DeclaredType::String => {
            writeln!(out, "{} = \"\";", id)
        }

        &types::DeclaredType::Integer => {
            writeln!(out, "{} = 0;", id)
        }

        &types::DeclaredType::RawPointer => {
            writeln!(out, "{} = NULL;", id)
        }

        _ => panic!("type `{}` cannot be default initialized!", target.decl_type)
    }
}

pub fn write_expr(out: &mut String, expr: &semantic::Expression)
    -> fmt::Result {
    match &expr.value {
        &node::ExpressionValue::BinaryOperator { ref lhs, ref op, ref rhs } => {
            let c_op = match op {
                &operators::Assignment => "=",
                &operators::Equals => "==",
                &operators::NotEquals => "!=",
                &operators::Minus => "-",
                &operators::Plus => "+",
                &operators::Deref => panic!("bad binary operator type: {}", op),
            };

            write!(out, "(")?;
            write_expr(out, lhs)?;
            write!(out, " {} ", c_op)?;
            write_expr(out, rhs)?;
            write!(out, ")")
        }

        &node::ExpressionValue::PrefixOperator { ref op, ref rhs } => {
            let c_op = match op {
                &operators::Plus => "+",
                &operators::Minus => "-",
                &operators::Deref => "*",
                &operators::Equals |
                &operators::NotEquals |
                &operators::Assignment => panic!("bad prefix operator type: {}", op),
            };

            write!(out, "(")?;
            write!(out, "{}", c_op)?;
            write_expr(out, rhs)?;
            write!(out, ")")
        }

        &node::ExpressionValue::FunctionCall { ref target, ref args } => {
            let args_str = args.iter()
                .map(|arg_expr| -> Result<String, fmt::Error> {
                    let mut expr_out = String::new();
                    write_expr(&mut expr_out, arg_expr)?;

                    Ok(expr_out)
                })
                .collect::<Result<Vec<_>, _>>()?
                .join(", ");

            let target_name = symbol_to_c(target);
            write!(out, "{}({})", target_name, args_str)
        }

        &node::ExpressionValue::LiteralInteger(ref i) => {
            write!(out, "(({}) {})", type_to_c(&types::DeclaredType::Integer), i)
        }

        &node::ExpressionValue::LiteralString(ref s) => {
            write!(out, "(({})\"{}\")", type_to_c(&types::DeclaredType::String), s)
        }

        &node::ExpressionValue::If { ref condition, ref then_branch, ref else_branch } => {
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

        &node::ExpressionValue::ForLoop { ref from, ref to, ref body } => {
            let iter_expr = match &from.as_ref().value {
                &node::ExpressionValue::BinaryOperator { ref lhs, ref op, .. } => {
                    assert_eq!(operators::Assignment, *op);
                    lhs.clone()
                },
                _ => panic!("for loop 'from' clause must be an assignment")
            };

            write!(out, "for (")?;
            write_expr(out, from)?;
            write!(out, ";")?;
            write_expr(out, iter_expr.as_ref())?;
            write!(out, " < ")?;
            write_expr(out, to)?;
            write!(out, "; ++")?;
            write_expr(out, iter_expr.as_ref())?;
            writeln!(out, ") {{")?;
            write_expr(out, body)?;
            writeln!(out, ";}}")
        }

        &node::ExpressionValue::Identifier(ref sym) => {
            write!(out, "{}", symbol_to_c(sym))
        }

        &node::ExpressionValue::Member { ref of, ref name } => {
            write_expr(out, of)?;
            write!(out, ".{}", name)
        }

        &node::ExpressionValue::Block(ref block) => {
            write_block(out, block)
        }
    }
}

pub fn write_block(out: &mut String, block: &semantic::Block)
    -> fmt::Result {
    writeln!(out, "{{")?;

    for statement in block.statements.iter() {
        write_expr(out, statement)?;
        writeln!(out, ";")?;
    }

    writeln!(out, "}}")
}

pub fn write_vars(out: &mut String, vars: &semantic::Vars) -> fmt::Result {
    vars.decls.iter()
        .map(|decl| {
            writeln!(out, "{} {};", type_to_c(&decl.decl_type), decl.name)
        })
        .collect()
}

pub fn default_initialize_vars(out: &mut String, vars: &semantic::Vars) -> fmt::Result {
    vars.decls.iter()
        .map(|decl| {
            default_initialize(out, &types::Symbol::new(decl.name.as_str(),
                                                        decl.decl_type.clone()))
        })
        .collect()
}

pub fn write_record_decl(out: &mut String, record_decl: &semantic::RecordDecl) -> fmt::Result {
    assert!(record_decl.members.len() > 0, "structs must have at least one member");

    writeln!(out, "struct {} {{", record_decl.name)?;
    for member in record_decl.members.iter() {
        writeln!(out, "{} {};", type_to_c(&member.decl_type), member.name)?;
    }
    writeln!(out, "}};")?;
    writeln!(out)
}

pub fn write_function(out: &mut String, function: &semantic::Function)
    -> fmt::Result {
    let return_type = function.return_type.as_ref()
        .map(type_to_c);

    write!(out, "{} ", return_type.clone().unwrap_or_else(|| "void".to_owned()))?;
    write!(out, "{} ", function.name)?;

    writeln!(out, "({}) {{", function.args.decls.iter()
        .map(|arg_decl| {
            format!("{} {}", type_to_c(&arg_decl.decl_type), &arg_decl.name)
        })
        .collect::<Vec<_>>()
        .join(", "))?;

    write_vars(out, &function.local_vars)?;
    default_initialize_vars(out, &function.local_vars)?;

    match return_type {
        Some(_) => writeln!(out, "return result;")?,
        None => (),
    }

    write_block(out, &function.body)?;
    writeln!(out, "}}")?;
    writeln!(out)
}

pub fn write_c(program: &semantic::Program)
    -> Result<String, fmt::Error> {
    let mut output = String::new();

    writeln!(output, "#include <stdint.h>")?;
    writeln!(output, "#include <stdlib.h>")?;
    writeln!(output, "#include <stdio.h>")?;
    writeln!(output, "#include <string.h>")?;
    writeln!(output, "#include <stdbool.h>")?;

    writeln!(output, "typedef int8_t System_Byte;")?;
    writeln!(output, "typedef int64_t System_Integer;")?;
    writeln!(output, "typedef const char* System_String;")?;
    writeln!(output, "typedef void* System_Pointer;")?;
    writeln!(output, "typedef bool System_Boolean;")?;


    writeln!(output,
r"static void System_WriteLn(System_String ln) {{
    if (!ln) abort();

    puts(ln);
}}

static System_Pointer System_GetMem(System_Integer bytes) {{
    if (bytes > 0) {{
        return malloc((size_t) bytes);
    }} else {{
        return NULL;
    }}
}}

static void System_FreeMem(System_Pointer p) {{
    free(p);
}}")?;

    for decl in program.decls.iter() {
        match decl {
            &node::UnitDeclaration::Function(ref func_decl) =>
                write_function(&mut output, func_decl)?,
            &node::UnitDeclaration::Record(ref record_decl) =>
                write_record_decl(&mut output, record_decl)?,
            &node::UnitDeclaration::Vars(ref vars_decl) =>
                write_vars(&mut output, vars_decl)?,
        }
    }

    let var_decls = program.decls.iter()
        .filter_map(|decl| match decl {
            &node::UnitDeclaration::Vars(ref vars_decl) => Some(vars_decl),
            _ => None
        });

    writeln!(output, "int main(int argc, char* argv[]) {{")?;

    for vars_decl in var_decls {
        default_initialize_vars(&mut output, vars_decl)?;
    }

    write_block(&mut output, &program.program_block)?;
    writeln!(output, "  return 0;")?;
    writeln!(output, "}}")?;

    Ok(output)
}