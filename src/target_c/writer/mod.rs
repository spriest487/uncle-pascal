mod module_globals;

use std::{
    fmt::{self, Write},
};

use operators;
use semantic::{
    self,
    ProgramModule,
    Scope,
};
use node::{
    self,
    ExpressionValue,
    Identifier,
    FunctionKind,
    RecordKind,
    UnitDeclaration,
};
use types::{
    Type,
    Symbol,
};
use self::module_globals::ModuleGlobals;
use super::{HEADER, RT};

pub fn type_to_c(pascal_type: &Type, scope: &Scope) -> String {
    match pascal_type {
        Type::Nil => panic!("cannot output `nil` as a type in C"),
        Type::Byte => "System_Byte".to_owned(),
        Type::Int32 => "System_Int32".to_owned(),
        Type::UInt32 => "System_UInt32".to_owned(),
        Type::Int64 => "System_Int64".to_owned(),
        Type::UInt64 => "System_UInt64".to_owned(),
        Type::NativeInt => "System_NativeInt".to_owned(),
        Type::NativeUInt => "System_NativeUInt".to_owned(),
        Type::Boolean => "System_Boolean".to_owned(),
        Type::RawPointer => "System_Pointer".to_owned(),
        Type::Pointer(target) => {
            let target_c = type_to_c(target.as_ref(), scope);
            format!("{}*", target_c)
        }
        Type::Function(name) => {
            let sig = scope.get_function(name)
                .expect("reference type must exist");
            let return_type = sig.return_type.as_ref()
                .map(|ty| type_to_c(ty, scope))
                .unwrap_or_else(|| "void".to_owned());
            let arg_types = sig.args.decls.iter()
                .map(|arg| type_to_c(&arg.decl_type, scope))
                .collect::<Vec<_>>()
                .join(", ");

            format!("({} (*{})({}))", return_type, name, arg_types)
        }
        Type::Class(name) => {
            let class = scope.get_class(name)
                .expect("referenced class must exist");

            format!("{}*", identifier_to_c(&class.name))
        }
        Type::Record(name) => {
            let record = scope.get_record(name)
                .expect("referenced record must exist");

            format!("{}", identifier_to_c(&record.name))
        }

        Type::Array(array) => {
            let element_name = type_to_c(array.element.as_ref(), scope);
            let element_count = array.total_elements();

            format!("System_Internal_Array<{}[{}]>", element_name, element_count)
        }
    }.to_owned()
}

pub fn identifier_to_c(id: &Identifier) -> String {
    let mut parts = id.namespace.clone();
    parts.push(id.name.clone());

    parts.join("_")
}

pub fn symbol_to_c(sym: &semantic::ScopedSymbol) -> String {
    match sym {
        semantic::ScopedSymbol::Local { name, .. } => {
            identifier_to_c(&name)
        }

        semantic::ScopedSymbol::RecordMember { record_id, name, .. } => {
            format!("{}.{}", identifier_to_c(record_id), name)
        }
    }
}

pub fn default_initialize(out: &mut String, target: &Symbol) -> fmt::Result {
    let id = identifier_to_c(&target.name);

    match &target.decl_type {
        Type::Array(_) |
        Type::Record(_) => {
            /* zero initializing works for both record instances and RCd classes */
            writeln!(out, "memset(&{}, 0, sizeof({}));", id, id)
        }

        num @ _ if num.is_numeric() => {
            writeln!(out, "{} = 0;", id)
        }

        Type::Class(_) |
        Type::RawPointer |
        Type::Pointer(_) => {
            writeln!(out, "{} = nullptr;", id)
        }

        _ => panic!("type `{}` cannot be default initialized!", target.decl_type)
    }
}

pub fn write_expr(out: &mut String,
                  expr: &semantic::Expression,
                  globals: &mut ModuleGlobals)
                  -> fmt::Result {
    match &expr.value {
        ExpressionValue::BinaryOperator { lhs, op, rhs } => {
            let string_type = Some(Type::Class(Identifier::from("System.String")));
            if *op == operators::Plus
                && lhs.expr_type().unwrap() == string_type
                && rhs.expr_type().unwrap() == string_type {
                write!(out, "System_StringConcat(")?;
                write_expr(out, lhs.as_ref(), globals)?;
                write!(out, ", ")?;
                write_expr(out, rhs.as_ref(), globals)?;
                write!(out, ")")?;

                return Ok(());
            }

            let c_op = match op {
                operators::Assignment => "=",
                operators::Equals => "==",
                operators::NotEquals => "!=",
                operators::Minus => "-",
                operators::Plus => "+",
                operators::Multiply => "*",
                operators::Divide => "/",
                operators::And => "&&",
                operators::Or => "||",
                operators::Lt => "<",
                operators::Lte => "<=",
                operators::Gt => ">",
                operators::Gte => ">=",

                operators::AddressOf |
                operators::Deref => panic!("bad binary operator type: {}", op),
            };

//            println!("BINARY OP {} @ {}", op, expr.context.location);
//            println!("  BINARY LHS: {:?}", lhs);
//            println!("  BINARY RHS: {:?}", rhs);

            write!(out, "(")?;
            write_expr(out, lhs.as_ref(), globals)?;
            write!(out, " {} ", c_op)?;
            write_expr(out, rhs.as_ref(), globals)?;
            write!(out, ")")
        }

        ExpressionValue::PrefixOperator { op, rhs } => {
            let c_op = match op {
                operators::Plus => "+",
                operators::Minus => "-",
                operators::Deref => "*",
                operators::AddressOf => "&",

                operators::And |
                operators::Or |
                operators::Equals |
                operators::NotEquals |
                operators::Multiply |
                operators::Divide |
                operators::Gt |
                operators::Gte |
                operators::Lt |
                operators::Lte |
                operators::Assignment => panic!("bad prefix operator type: {}", op),
            };

//            println!("PREFIX OP {} @ {}", op, expr.context.location);
//            println!("  PREFIX RHS: {:?}", rhs);

            write!(out, "(")?;
            write!(out, "{}", c_op)?;
            write_expr(out, rhs, globals)?;
            write!(out, ")")
        }

        ExpressionValue::FunctionCall { target, args } => {
            let args_str = args.iter()
                .map(|arg_expr| -> Result<String, fmt::Error> {
                    let mut expr_out = String::new();
                    write_expr(&mut expr_out, arg_expr, globals)?;

                    Ok(expr_out)
                })
                .collect::<Result<Vec<_>, _>>()?
                .join(", ");

            write_expr(out, target, globals)?;
            write!(out, "({})", args_str)
        }

        ExpressionValue::LetBinding { name, value } => {
            let value_type = value.expr_type().unwrap().unwrap();

            write!(out, "{} {} =", type_to_c(&value_type, expr.scope()), name)?;

            write_expr(out, value, globals)
        }

        ExpressionValue::LiteralInteger(i) => {
            write!(out, "(({}) {})", type_to_c(&Type::Int64, expr.scope()), i)
        }

        ExpressionValue::LiteralString(s) => {
            let name = globals.string_literal_name(s);
            write!(out, "{}", name)
        }

        ExpressionValue::LiteralNil => {
            out.write_str("NULL")
        }

        ExpressionValue::If { condition, then_branch, else_branch } => {
            write!(out, "if (")?;
            write_expr(out, condition, globals)?;
            write!(out, ") {{")?;

            write_expr(out, then_branch, globals)?;
            writeln!(out, "; }}")?;

            if let &Some(ref else_expr) = else_branch {
                write!(out, " else {{")?;
                write_expr(out, else_expr, globals)?;
                writeln!(out, "; }}")?;
            }
            Ok(())
        }

        ExpressionValue::ForLoop { from, to, body } => {
            let iter_expr = match &from.as_ref().value {
                ExpressionValue::BinaryOperator { lhs, op, .. }
                if *op == operators::Assignment => {
                    let mut iter_expr_out = String::new();
                    write_expr(&mut iter_expr_out, lhs, globals)?;
                    iter_expr_out
                }
                ExpressionValue::LetBinding { ref name, .. } => {
                    name.clone()
                }
                _ => panic!("for loop 'from' clause must be an assignment or a let binding")
            };

            write!(out, "for (")?;
            write_expr(out, from, globals)?;
            write!(out, "; {} < ", iter_expr)?;
            write_expr(out, to, globals)?;
            writeln!(out, "; {} += 1) {{", iter_expr)?;
            write_expr(out, body, globals)?;
            writeln!(out, ";}}")
        }

        ExpressionValue::Identifier(ref sym) => {
            write!(out, "{}", symbol_to_c(sym))
        }

        ExpressionValue::Member { ref of, ref name } => {
            let mut member_out = String::new();
            //panic!("of: {:?}, name: {}", of, name);
            let mut of_type: Type = of.expr_type()
                .unwrap()
                .expect("target of member expression must exist");

            let mut deref_levels = 0;
            loop {
                match of_type {
                    Type::Pointer(of_target) => {
                        write!(member_out, "(*")?;
                        deref_levels += 1;
                        of_type = *of_target;
                    }

                    _ => {
                        write_expr(&mut member_out, of, globals)?;
                        break;
                    }
                }
            }

            for _ in 0..deref_levels {
                write!(member_out, ")")?;
            }

            if of_type.is_class() {
                write!(member_out, "->{}", name)?;
            } else {
                write!(member_out, ".{}", name)?;
            }

            out.write_str(&member_out)
        }

        ExpressionValue::Block(block) => {
            write_block(out, block, globals)
        }
    }
}

pub fn write_block(out: &mut String,
                   block: &semantic::Block,
                   globals: &mut ModuleGlobals)
                   -> fmt::Result {
    writeln!(out, "{{")?;

    for statement in block.statements.iter() {
        write_statement(out, statement, globals)?;
    }

    // release all references
    for stmt in block.statements.iter() {
        if let ExpressionValue::LetBinding { name, value } = &stmt.value {
            if value.expr_type().unwrap().unwrap().is_class() {
                writeln!(out, "System_Internal_Rc_Release({});", name)?;
            }
        }
    }

    writeln!(out, "}}")
}

fn write_statement(out: &mut String,
                   statement: &semantic::Expression,
                   globals: &mut ModuleGlobals) -> fmt::Result {
    let mut bindings = Vec::new();
    let mut next_binding = 1;

    let stmt_after_let = match statement.clone() {
        // if it's a let-binding, declare the variable and turn it into a assignment
        semantic::Expression { value: ExpressionValue::LetBinding { name, value }, context } => {
            let binding_type: Type = value.expr_type()
                .expect("let binding target must be a valid type")
                .expect("let binding type must not be None");
            writeln!(out, "{} {};", type_to_c(&binding_type, value.scope()), name)?;

            let binding_id = Identifier::from(&name);
            default_initialize(out, &Symbol {
                name: binding_id.clone(),
                decl_type: binding_type.clone(),
            })?;

            let binding_sym = semantic::ScopedSymbol::Local {
                name: binding_id,
                decl_type: binding_type,
            };

            let context: semantic::SemanticContext = context;
            let binding_id_expr = semantic::Expression::identifier(binding_sym, context.clone());

            let binary_op = semantic::Expression::binary_op(binding_id_expr,
                                                            operators::Assignment,
                                                            *value,
                                                            context);
            binary_op
        }

        stmt @ _ => stmt,
    };

    /* bind the results of any function calls which return RC objects to temporary bindings so we
     can release them later. nothing but function calls returning RC objects can leak an RC,
     because local vars and func args are already managed elsewhere */
    let stmt_simplified = node::transform_expressions(stmt_after_let, &mut |subexpr: semantic::Expression| {
        // call expression...
        let call_target = match subexpr.value.clone() {
            ExpressionValue::FunctionCall { target, .. } => {
                target
            }
            _ => return subexpr,
        };

        // calling a function...
        let call_func = call_target.function_type()
            .expect("called function must have a valid type")
            .expect("called function target must be of function type");

        // returning an instance of an rc class..
        let class_type = match &call_func.return_type {
            Some(Type::Class(class_name)) => Type::Class(class_name.clone()),
            _ => return subexpr,
        };

        let name = format!("internal_rc_binding_{}", next_binding);
        next_binding += 1;

        let binding_sym = semantic::ScopedSymbol::Local {
            name: Identifier::from(&name),
            decl_type: class_type.clone(),
        };
        let binding_context = subexpr.context.clone();

        bindings.push((name, subexpr));

        semantic::Expression::identifier(binding_sym, binding_context)
    });

    if bindings.len() > 0 {
        //write a block around this statement for the scope of the temp bindings
        writeln!(out, "{{")?;

        for (tmp_name, tmp_val) in bindings.iter() {
            let tmp_type = tmp_val.expr_type()
                .expect("temporary rc values should always be valid types")
                .expect("temporary rc values should never have no type");
            let val_c_type = type_to_c(&tmp_type, tmp_val.scope());

            write!(out, "{} {} =", val_c_type, tmp_name)?;
            write_expr(out, tmp_val, globals)?;
            writeln!(out, ";")?;
        }
    }

    //write the statement body
    match &stmt_simplified.value {
        /* if assigning a new value to an rc variable, release the old value first */
        ExpressionValue::BinaryOperator { lhs, op, .. } => {
            let is_class_assignment = match lhs.class_type().unwrap() {
                Some(_) => *op == operators::Assignment,
                _ => false,
            };

            let mut lhs_expr = String::new();
            write_expr(&mut lhs_expr, &lhs, globals)?;

            /* release the old value of the variable, if it's initialized */
            if is_class_assignment {
                writeln!(out, "if ({} && {}->StrongCount > 0) {{", lhs_expr, lhs_expr)?;
                writeln!(out, " System_Internal_Rc_Release({});", lhs_expr)?;
                writeln!(out, "}}")?;
            }

            // do the thing
            write_expr(out, &stmt_simplified, globals)?;
            writeln!(out, ";")?;

            if is_class_assignment {
                /* retain rc variables: if it came from a function call, it'll be released once
                (from the code above which releases all rc results from functions), and if it's an
                existing value we now have two references to it */
                writeln!(out, "System_Internal_Rc_Retain({});", lhs_expr)?;
            }
        }

        _ => {
            write_expr(out, &stmt_simplified, globals)?;
            writeln!(out, ";")?;
        }
    }

    if bindings.len() > 0 {
        //release temp bindings and close the block
        for (tmp_name, _tmp_val) in bindings.iter() {
            writeln!(out, "System_Internal_Rc_Release({});", tmp_name)?;
        }
        writeln!(out, "}}")?;
    }

    Ok(())
}

pub fn write_vars(out: &mut String, vars: &semantic::Vars) -> fmt::Result {
    vars.decls.iter()
        .map(|decl| {
            writeln!(out, "{} {};",
                     type_to_c(&decl.decl_type, decl.scope()),
                     identifier_to_c(&decl.name))
        })
        .collect()
}

pub fn release_vars<'a>(out: &mut String,
                        vars: impl IntoIterator<Item=&'a semantic::VarDecl>)
                        -> fmt::Result {
    for decl in vars {
        if decl.decl_type.is_class() {
            writeln!(out, "System_Internal_Rc_Release({});", decl.name)?;
        }
    }

    Ok(())
}

pub fn default_initialize_vars<'a>(out: &mut String,
                                   decls: impl IntoIterator<Item=&'a semantic::VarDecl>)
                                   -> fmt::Result {
    decls.into_iter()
        .map(|decl| {
            default_initialize(out, &Symbol::new(decl.name.clone(),
                                                 decl.decl_type.clone()))
        })
        .collect()
}

pub fn write_record_decl(out: &mut String, record_decl: &semantic::RecordDecl) -> fmt::Result {
    assert!(record_decl.members.len() > 0, "structs must have at least one member");


    if record_decl.kind == RecordKind::Class {
        writeln!(out, "struct {}: System_Internal_Object {{", identifier_to_c(&record_decl.name))?;
    } else {
        writeln!(out, "struct {} {{", identifier_to_c(&record_decl.name))?;
    }

    for member in record_decl.members.iter() {
        writeln!(out, "{} {};",
                 type_to_c(&member.decl_type, record_decl.scope()),
                 identifier_to_c(&member.name))?;
    }
    writeln!(out, "}};")?;
    writeln!(out)
}

pub fn write_function(out: &mut String,
                      function: &semantic::FunctionDecl,
                      globals: &mut ModuleGlobals)
                      -> fmt::Result {
    let return_type_c = function.return_type.as_ref()
        .map(|return_type| type_to_c(return_type, function.scope()));

    write!(out, "{} ", return_type_c.clone().unwrap_or_else(|| "void".to_owned()))?;
    write!(out, "{} ", identifier_to_c(&function.name))?;

    write!(out, "({})", function.args.decls.iter()
        .map(|arg_decl| {
            format!("{} {}", type_to_c(&arg_decl.decl_type, arg_decl.scope()),
                    identifier_to_c(&arg_decl.name))
        })
        .collect::<Vec<_>>()
        .join(", "))?;

    match &function.body {
        None => writeln!(out, ";")?,

        Some(node::FunctionDeclBody { block, local_vars }) => {
            writeln!(out, "{{")?;

            write_vars(out, &local_vars)?;
            default_initialize_vars(out, local_vars.decls.iter())?;

            if function.kind == FunctionKind::Constructor {
                //the actual return type is an Rc, but we need to pass the class type to sizeof
                let constructed_class_c_name = match function.return_type.as_ref().unwrap() {
                    Type::Class(name) => {
                        identifier_to_c(&name)
                    }
                    _ => panic!("constructor must return a class type"),
                };

                writeln!(out, "result = ({}*)System_Internal_Rc_GetMem(sizeof(struct {}), \"{}\");",
                         constructed_class_c_name,
                         constructed_class_c_name,
                         function.return_type.as_ref().expect("constructor must have return type"))?;
            }

            let rc_args: Vec<_> = function.args.decls.iter()
                .filter_map(|arg| {
                    if let Type::Class(_) = &arg.decl_type {
                        return Some(arg);
                    }
                    None
                }).collect();

            // retain rc args to non-destructor functions
            // this is kind of a hack to make sure temporary values used as args get rc'd
            // destructors musn't change the ref count of their only arg, the dead object, or bad things
            // happen
            // it's safe to do this with a constructor because the object under construction exists only
            // in the result position
            if function.kind == FunctionKind::Function {
                for arg in rc_args.iter() {
                    writeln!(out, "System_Internal_Rc_Retain({});", identifier_to_c(&arg.name))?;
                }
            }

            write_block(out, block, globals)?;

            //release all local vars except the result
            let result_id = Identifier::from("result");
            release_vars(out, local_vars.decls.iter()
                .filter(|decl| decl.name != result_id))?;

            if function.kind == FunctionKind::Function {
                //release args
                for arg in rc_args.iter() {
                    writeln!(out, "System_Internal_Rc_Release({});", arg.name)?;
                }
            }

            match return_type_c {
                Some(_) => writeln!(out, "return result;")?,
                None => (),
            }

            writeln!(out, "}}")?;
            writeln!(out)?;
        }
    }

    Ok(())
}

pub fn write_decl(out: &mut String,
                  decl: &semantic::UnitDeclaration,
                  globals: &mut ModuleGlobals) -> fmt::Result {
    match decl {
        UnitDeclaration::Function(ref func_decl) =>
            write_function(out, func_decl, globals),
        UnitDeclaration::Type(ref type_decl) =>
            match type_decl {
                node::TypeDecl::Record(record_decl) =>
                    write_record_decl(out, record_decl),

                node::TypeDecl::Alias { .. } => {
                    //aliases don't do anything in the final output and can be ignored
                    Ok(())
                }
            }
        UnitDeclaration::Vars(ref vars_decl) =>
            write_vars(out, vars_decl),
    }
}

pub fn write_static_init<'a>(out: &mut String,
                             decls: &'a Vec<semantic::UnitDeclaration>)
                             -> fmt::Result {
    let classes = decls.iter()
        .filter_map(|decl| {
            match decl {
                UnitDeclaration::Type(node::TypeDecl::Record(record))
                if record.kind == RecordKind::Class => {
                    Some(record)
                }

                _ => None,
            }
        });

    for class in classes {
        /* string is magically initialized earlier */
        if class.name.to_string() == "System.String" {
            continue;
        }

        let destructor = decls.iter()
            .filter_map(|decl| match decl {
                UnitDeclaration::Function(func_decl)
                if func_decl.is_destructor_of(&class)
                => Some(func_decl),
                _ => None
            })
            .next();

        let destructor_ptr = match destructor {
            Some(destructor) => format!("(System_Internal_Destructor)&{}", identifier_to_c(&destructor.name)),
            None => "nullptr".to_string(),
        };

        let class: &semantic::RecordDecl = class;
        writeln!(out, "System_Internal_InitClass(\"{}\", {});", class.name, destructor_ptr)?;
    }

    Ok(())
}

pub fn write_c(module: &ProgramModule)
               -> Result<String, fmt::Error> {
    let mut globals = ModuleGlobals::new();

    let mut c_decls = String::new();

    for unit in module.units.iter() {
        writeln!(c_decls, "/* {} interface */", unit.name)?;

        for decl in unit.interface.iter() {
            write_decl(&mut c_decls, decl, &mut globals)?;
        }
    }

    for unit in module.units.iter() {
        writeln!(c_decls, "/* {} implementation */", unit.name)?;
        for decl in unit.implementation.iter() {
            write_decl(&mut c_decls, decl, &mut globals)?;
        }
    }

    writeln!(c_decls, "/* program decls */")?;
    for decl in module.program.decls.iter() {
        write_decl(&mut c_decls, decl, &mut globals)?;
    }

    let var_decls: Vec<_> = module.program.decls.iter()
        .filter_map(|decl| match decl {
            UnitDeclaration::Vars(ref vars_decl) => Some(vars_decl),
            _ => None
        })
        .flat_map(|decl_group| decl_group.decls.iter())
        .collect();

    /* write main function */
    let mut c_main = String::new();

    writeln!(c_main, "/* program block */")?;

    default_initialize_vars(&mut c_main, var_decls.iter().cloned())?;

    write_block(&mut c_main, &module.program.program_block, &mut globals)?;
    release_vars(&mut c_main, var_decls)?;

    /* at this point the whole program is finished writing so globals should
    be ready to output */

    // combine sections into whole program
    let mut output = String::new();

    output.write_str(HEADER)?;

    // forward declare System.String
    writeln!(output, "struct System_String;")?;
    // declare initialize string literal global vars
    globals.declare_string_literals(&mut output)?;

    // write program decls
    output.write_str(&c_decls)?;

    writeln!(output, "int main(int argc, char* argv[]) {{")?;

    // init the string class and string literals
    writeln!(output, "System_Internal_InitClass(\"System.String\", (System_Internal_Destructor)&System_DestroyString);")?;
    globals.init_string_literals(&mut output)?;

    // init other classes
    for unit in module.units.iter() {
        write_static_init(&mut output, &unit.interface)?;
        write_static_init(&mut output, &unit.implementation)?;
    }
    write_static_init(&mut output, &module.program.decls)?;

    output.write_str(&c_main)?;

    writeln!(output, "return 0;")?;
    writeln!(output, "}}")?;

    output.write_str(RT)?;

    Ok(output)
}
