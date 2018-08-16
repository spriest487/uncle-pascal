use std::fmt::{self, Write};

use semantic;
use operators;
use node;
use types;
use ProgramModule;

pub fn type_to_c(pascal_type: &types::DeclaredType) -> String {
    match pascal_type {
        &types::DeclaredType::Nil => panic!("cannot output `nil` as a type in C"),
        &types::DeclaredType::Byte => "System_Byte".to_owned(),
        &types::DeclaredType::Integer => "System_Integer".to_owned(),
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
        }
        &types::DeclaredType::Record(ref decl) => {
            match decl.kind {
                types::RecordKind::Class => format!("/* {} */ struct System_Internal_Rc", decl.name),
                types::RecordKind::Record => format!("struct {}", identifier_to_c(&decl.name)),
            }
        }
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
            identifier_to_c(&name)
        }

        &semantic::ScopedSymbol::RecordMember { ref record_id, ref name, .. } => {
            format!("{}.{}", identifier_to_c(record_id), name)
        }
    }
}

pub fn default_initialize(out: &mut String, target: &types::Symbol) -> fmt::Result {
    let id = identifier_to_c(&target.name);

    match &target.decl_type {
        &types::DeclaredType::Record(ref _decl) => {
            /* zero initializing works for both record instances and RCd classes */
            writeln!(out, "memset(&{}, 0, sizeof({}));", id, id)
        }

        &types::DeclaredType::Integer => {
            writeln!(out, "{} = 0;", id)
        }

        &types::DeclaredType::RawPointer |
        &types::DeclaredType::Pointer(_) => {
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

                &operators::AddressOf |
                &operators::Deref => panic!("bad binary operator type: {}", op),
            };

//            println!("BINARY OP {} @ {}", op, expr.context.location);
//            println!("  BINARY LHS: {:?}", lhs);
//            println!("  BINARY RHS: {:?}", rhs);

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
                &operators::AddressOf => "&",

                &operators::Equals |
                &operators::NotEquals |
                &operators::Assignment => panic!("bad prefix operator type: {}", op),
            };

//            println!("PREFIX OP {} @ {}", op, expr.context.location);
//            println!("  PREFIX RHS: {:?}", rhs);

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

            write_expr(out, target)?;
            write!(out, "({})", args_str)
        }

        &node::ExpressionValue::LetBinding { ref name, ref value } => {
            let value_type = value.expr_type().unwrap().unwrap();

            write!(out, "{} {} =", type_to_c(&value_type), name)?;
            write_expr(out, value)
        }

        &node::ExpressionValue::LiteralInteger(ref i) => {
            write!(out, "(({}) {})", type_to_c(&types::DeclaredType::Integer), i)
        }

        &node::ExpressionValue::LiteralString(ref s) => {
            write!(out, "(({})\"{}\")", type_to_c(&types::DeclaredType::Byte.pointer()), s)
        }

        &node::ExpressionValue::LiteralNil => {
            out.write_str("NULL")
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
                &node::ExpressionValue::BinaryOperator { ref lhs, ref op, .. }
                if *op == operators::Assignment => {
                    let mut iter_expr_out = String::new();
                    write_expr(&mut iter_expr_out, lhs)?;
                    iter_expr_out
                }
                &node::ExpressionValue::LetBinding { ref name, .. } => {
                    name.clone()
                }
                _ => panic!("for loop 'from' clause must be an assignment or a let binding")
            };

            write!(out, "for (")?;
            write_expr(out, from)?;
            write!(out, "; {} < ", iter_expr)?;
            write_expr(out, to)?;
            writeln!(out, "; {} += 1) {{", iter_expr)?;
            write_expr(out, body)?;
            writeln!(out, ";}}")
        }

        &node::ExpressionValue::Identifier(ref sym) => {
            write!(out, "{}", symbol_to_c(sym))
        }

        &node::ExpressionValue::Member { ref of, ref name } => {
            let mut member_out = String::new();
            //panic!("of: {:?}, name: {}", of, name);
            let mut of_type: types::DeclaredType = of.expr_type()
                .unwrap()
                .expect("target of member expression must exist");

            let mut deref_levels = 0;
            loop {
                match of_type {
                    types::DeclaredType::Pointer(of_target) => {
                        write!(member_out, "(*")?;
                        deref_levels += 1;
                        of_type = *of_target;
                    }

                    _ => {
                        write_expr(&mut member_out, of)?;
                        break;
                    }
                }
            }

            for _ in 0..deref_levels {
                write!(member_out, ")")?;
            }

            match &of_type {
                types::DeclaredType::Record(decl) if decl.kind == types::RecordKind::Class => {
                    let of_type_c = type_to_c(&of_type);
                    let c_class_struct_name = format!("struct {}", identifier_to_c(&decl.name));

                    member_out.insert_str(0, &format!("(({}*)", c_class_struct_name));
                    write!(member_out, ".Value)->{}", name)?;
                }
                _ => {
                    write!(member_out, ".{}", name)?;
                }
            };

            out.write_str(&member_out)
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
            writeln!(out, "{} {};",
                     type_to_c(&decl.decl_type),
                     identifier_to_c(&decl.name))
        })
        .collect()
}

pub fn default_initialize_vars(out: &mut String, vars: &semantic::Vars) -> fmt::Result {
    vars.decls.iter()
        .map(|decl| {
            default_initialize(out, &types::Symbol::new(decl.name.clone(),
                                                        decl.decl_type.clone()))
        })
        .collect()
}

pub fn write_record_decl(out: &mut String, record_decl: &semantic::RecordDecl) -> fmt::Result {
    assert!(record_decl.members.len() > 0, "structs must have at least one member");

    writeln!(out, "struct {} {{", identifier_to_c(&record_decl.name))?;
    for member in record_decl.members.iter() {
        writeln!(out, "{} {};",
                 type_to_c(&member.decl_type),
                 identifier_to_c(&member.name))?;
    }
    writeln!(out, "}};")?;
    writeln!(out)
}

pub fn write_function(out: &mut String, function: &semantic::Function)
                      -> fmt::Result {
    let return_type_c = function.return_type.as_ref()
        .map(type_to_c);

    write!(out, "{} ", return_type_c.clone().unwrap_or_else(|| "void".to_owned()))?;
    write!(out, "{} ", identifier_to_c(&function.name))?;

    writeln!(out, "({}) {{", function.args.decls.iter()
        .map(|arg_decl| {
            format!("{} {}", type_to_c(&arg_decl.decl_type),
                    identifier_to_c(&arg_decl.name))
        })
        .collect::<Vec<_>>()
        .join(", "))?;

    write_vars(out, &function.local_vars)?;
    default_initialize_vars(out, &function.local_vars)?;

    if function.constructor {
        //the actual return type is an Rc
        writeln!(out, "result = System_Internal_Rc_GetMem(sizeof({}), \"{}\");",
                 return_type_c.as_ref().expect("constructor must have valid C return type"),
                 function.return_type.as_ref().expect("constructor must have return type"))?;
    }

    write_block(out, &function.body)?;

    match return_type_c {
        Some(_) => writeln!(out, "return result;")?,
        None => (),
    }

    writeln!(out, "}}")?;
    writeln!(out)
}

pub fn write_decl(out: &mut String, decl: &semantic::UnitDeclaration) -> fmt::Result {
    match decl {
        &node::UnitDeclaration::Function(ref func_decl) =>
            write_function(out, func_decl),
        &node::UnitDeclaration::Record(ref record_decl) =>
            write_record_decl(out, record_decl),
        &node::UnitDeclaration::Vars(ref vars_decl) =>
            write_vars(out, vars_decl),
    }
}

pub fn write_c(module: &ProgramModule)
               -> Result<String, fmt::Error> {
    let mut output = String::new();

    output.write_str(include_str!("header.h"))?;

    for unit in module.units.iter() {
        writeln!(output, "/* {} interface */", unit.name)?;

        for decl in unit.interface.iter() {
            write_decl(&mut output, decl)?;
        }
    }

    for unit in module.units.iter() {
        writeln!(output, "/* {} implementation */", unit.name)?;
        for decl in unit.implementation.iter() {
            write_decl(&mut output, decl)?;
        }
    }

    writeln!(output, "/* program decls */")?;
    for decl in module.program.decls.iter() {
        write_decl(&mut output, decl)?;
    }

    writeln!(output, "/* program vars */")?;

    let var_decls = module.program.decls.iter()
        .filter_map(|decl| match decl {
            &node::UnitDeclaration::Vars(ref vars_decl) => Some(vars_decl),
            _ => None
        });

    writeln!(output, "/* program main */")?;

    writeln!(output, "int main(int argc, char* argv[]) {{")?;

    for vars_decl in var_decls {
        default_initialize_vars(&mut output, vars_decl)?;
    }

    write_block(&mut output, &module.program.program_block)?;
    writeln!(output, "  return 0;")?;
    writeln!(output, "}}")?;

    Ok(output)
}