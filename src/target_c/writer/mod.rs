mod module_globals;

use std::{
    fmt::{self, Write},
    rc::Rc,
};

use operators;
use semantic::{
    self,
    SemanticContext,
    ProgramModule,
    Scope,
    BindingKind,
};
use node::{
    self,
    ToSource,
    Context,
    ExpressionValue,
    Identifier,
    FunctionKind,
    RecordKind,
    UnitDecl,
    Implementation,
    FunctionArgModifier,
    FunctionLocalDecl,
    ConstantExpression,
};
use types::{
    Type,
    TypedSymbol,
};
use consts::{
    IntConstant,
    FloatConstant,
};

use self::module_globals::ModuleGlobals;
use super::{HEADER, RT};

pub fn type_to_c(pascal_type: &Type, scope: &Scope) -> String {
    match pascal_type {
        Type::Nil => panic!("cannot output `nil` as a type in C"),
        Type::Byte => "System_Byte".to_string(),
        Type::Int32 => "System_Int32".to_string(),
        Type::UInt32 => "System_UInt32".to_string(),
        Type::Int64 => "System_Int64".to_string(),
        Type::UInt64 => "System_UInt64".to_string(),
        Type::NativeInt => "System_NativeInt".to_string(),
        Type::NativeUInt => "System_NativeUInt".to_string(),
        Type::Float64 => "System_Float64".to_string(),
        Type::Boolean => "System_Boolean".to_string(),
        Type::RawPointer => "System_Pointer".to_string(),
        Type::UntypedRef => "void*".to_string(),
        Type::Pointer(target) => {
            let target_c = type_to_c(target.as_ref(), scope);
            format!("{}*", target_c)
        }
        Type::Function(sig) => {
            let return_type = sig.return_type.as_ref()
                .map(|ty| type_to_c(ty, scope))
                .unwrap_or_else(|| "void".to_owned());
            let arg_types = if sig.arg_types.len() > 0 {
                sig.arg_types.iter()
                    .map(|arg| type_to_c(&arg, scope))
                    .collect::<Vec<_>>()
                    .join(", ")
            } else {
                "void".to_string()
            };

            format!("System_Internal_Func<{}, {}>", return_type, arg_types)
        }
        Type::Class(name) => {
            let (class_id, _) = scope.get_class(name)
                .expect("referenced class must exist");

            format!("{}*", identifier_to_c(&class_id))
        }
        Type::Record(name) => {
            let (record_id, _) = scope.get_record(name)
                .expect("referenced record must exist");

            format!("{}", identifier_to_c(&record_id))
        }

        Type::Enumeration(enum_id) => {
            let (enum_id, _) = scope.get_enumeration(enum_id)
                .expect("referenced enumeration must exist");
            identifier_to_c(&enum_id)
        }

        Type::Set(set_id) => {
            let (set_id, _) = scope.get_set(set_id)
                .expect("referenced set must exist");
            identifier_to_c(&set_id)
        }

        Type::DynamicArray(dynamic_array_type) => {
            let element_type = type_to_c(&dynamic_array_type.element, scope);
            format!("std::shared_ptr<{}[]>", element_type)
        }

        Type::Array(array) => {
            let element_name = type_to_c(array.element.as_ref(), scope);
            let element_count = array.total_elements();

            format!("System_Internal_Array<{}[{}]>", element_name, element_count)
        }
    }
}

pub fn identifier_to_c(id: &Identifier) -> String {
    let mut parts = id.namespace.clone();
    parts.push(id.name.clone());

    parts.join("_")
}

pub fn default_initialize(out: &mut String, target: &TypedSymbol) -> fmt::Result {
    let id = identifier_to_c(&target.name);

    match &target.decl_type {
        Type::Array(_) |
        Type::Record(_) => {
            /* zero initializing works for both record instances and RCd classes */
            writeln!(out, "memset(&{}, 0, sizeof({}));", id, id)
        }

        Type::Boolean => {
            writeln!(out, "{} = false;", id)
        }

        num @ _ if num.is_numeric() => {
            writeln!(out, "{} = 0;", id)
        }

        Type::Enumeration(_enum_id) => {
            //todo: should use the first ordinal in the enum, not 0
            writeln!(out, "{} = 0;", id)
        }

        Type::Set(_) => {
            // sets can always be empty
            writeln!(out, "{} = 0;", id)
        }

        Type::Class(_) |
        Type::RawPointer |
        Type::Pointer(_) => {
            writeln!(out, "{} = nullptr;", id)
        }

        Type::Function(_sig) => {
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

            if *op == operators::In {
                unimplemented!("in operator (c++ backend")
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

                operators::Not |
                operators::RangeInclusive |
                operators::AddressOf |
                operators::Deref => panic!("bad binary operator type: {}", op),

                operators::In => unreachable!("special cased above"),
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
                operators::Not => "!",

                operators::In |
                operators::RangeInclusive |
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

        ExpressionValue::TypeCast { target_type, from_value } => {
            let target_ctype = type_to_c(target_type, expr.scope());

            write!(out, "static_cast<{}>(", target_ctype)?;
            write_expr(out, from_value, globals)?;
            write!(out, ")")
        }

        ExpressionValue::LetBinding { name, value } => {
            let value_type = value.expr_type().unwrap().unwrap();

            write!(out, "{} {} =", type_to_c(&value_type, expr.scope()), name)?;

            write_expr(out, value, globals)
        }

        ExpressionValue::Constant(const_expr) => {
            match const_expr {
                ConstantExpression::Integer(i) => {
                    let int_type = const_expr.value_type();
                    write!(out, "(({})", type_to_c(&int_type, expr.scope()))?;
                    match i {
                        IntConstant::U32(i) => write!(out, "0x{:x}", i)?,
                        IntConstant::U64(i) => write!(out, "0x{:x}", i)?,
                        IntConstant::I32(i) => write!(out, "{}", i)?,
                        IntConstant::I64(i) => write!(out, "{}ll", i)?,
                        IntConstant::Char(c) => write!(out, "{}", c)?,
                    }
                    write!(out, ")")
                }

                ConstantExpression::Enum(e) => {
                    let enum_name = identifier_to_c(&e.enumeration);
                    write!(out, "(({}){})", enum_name, e.ordinal)
                }

                ConstantExpression::Set(set) => {
                    let (set_id, _set_decl) = expr.scope().get_set(&set.set)
                        .unwrap();

                    let enum_names = expr.scope().get_set_enumeration(&set_id)
                        .unwrap();

                    let set_mask = enum_names.iter()
                        .enumerate()
                        .filter_map(|(val_ordinal, val_name)| {
                            if set.included_values.contains(val_name) {
                                let val_mask = 1 << val_ordinal;
                                Some(format!("{}", val_mask))
                            } else {
                                None
                            }
                        })
                        .collect::<Vec<_>>()
                        .join(" | ");

                    write!(out, "(({}) {})", identifier_to_c(&set_id), set_mask)
                }

                ConstantExpression::Float(f) => {
                    let float_type = const_expr.value_type();
                    write!(out, "(({})", type_to_c(&float_type, expr.scope()))?;
                    match f {
                        FloatConstant::F64(val) => write!(out, "{:e}", val)?
                    }
                    write!(out, ")")
                }

                ConstantExpression::String(s) => {
                    let name = globals.string_literal_name(s);
                    write!(out, "{}", name)
                }

                ConstantExpression::Boolean(val) => {
                    write!(out, "{}", if *val { "true" } else { "false" })
                }

                ConstantExpression::Nil => {
                    out.write_str("NULL")
                }
            }
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

        ExpressionValue::While { condition, body } => {
            write!(out, "while (")?;
            write_expr(out, condition, globals)?;
            writeln!(out, ") {{")?;
            write_expr(out, body, globals)?;
            writeln!(out, "}}")
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

        ExpressionValue::Identifier(sym) => {
            write!(out, "{}", identifier_to_c(sym))
        }

        ExpressionValue::ArrayElement { of, index_expr } => {
            write!(out, "(")?;
            write_expr(out, of, globals)?;
            write!(out, "[")?;
            write_expr(out, index_expr, globals)?;
            write!(out, "])")
        }

        ExpressionValue::Member { of, name } => {
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

        ExpressionValue::SetConstructor(_member_groups) => {
            unimplemented!("set constructor (c++ backend)");
        }

        ExpressionValue::With { .. } => {
            unreachable!("with statements should be removed during semantic analysis");
        }

        ExpressionValue::Raise(error) => {
            let location = &expr.context.token().location;
            let file = location.file.replace("\"", "\\\"");
            let msg = error.to_source().replace("\"", "\\\"");
            /* with absolutely no exception support the best thing we can do is convert
            the error to a string */
            write!(out, "System_Internal_Raise(\"{}\", {}, {}, \"{}\")",
                   file,
                   location.line,
                   location.col,
                   msg)
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
    if let ExpressionValue::Block(block) = &statement.value {
        return write_block(out, block, globals);
    }

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
            default_initialize(out, &TypedSymbol {
                name: binding_id.clone(),
                decl_type: binding_type.clone(),
            })?;

            /* the scope of the assignment needs the let-bound variable added to it,
            because a let binding doesn't have itself in scope */
            let context = semantic::SemanticContext::new(
                context.token().clone(),
                Rc::new(statement.scope().clone()
                    .with_symbol_local(&name, binding_type, BindingKind::Immutable)),
            );
            let binding_id_expr = semantic::Expression::identifier(binding_id, context.clone());

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
        if !call_func.return_type.as_ref().map(|t| t.is_class()).unwrap_or(false) {
            return subexpr;
        }

        let name = format!("internal_rc_binding_{}", next_binding);
        next_binding += 1;

        /* the temp binding needs to be added to scope in case anything typechecks again after
        this point*/
        let binding_scope = {
            let decl_type = call_func.return_type.as_ref().cloned().unwrap();

            subexpr.scope().clone()
                .with_symbol_local(&name, decl_type, BindingKind::Immutable)
        };

        let binding_context = SemanticContext::new(subexpr.context.token().clone(), binding_scope);

        /* construct a new expression referring to the temp binding instead of the original
        expression by its name */
        let binding_expr = semantic::Expression::identifier(Identifier::from(&name),
                                                            binding_context);

        /* store the original expression value for later when we wriite out the temp bindings */
        bindings.push((name, subexpr));
        binding_expr
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

pub fn write_consts(out: &mut String,
                    consts: &semantic::ConstDecls,
                    globals: &mut ModuleGlobals)
                    -> fmt::Result {
    consts.decls.iter()
        .map(|decl| {
            let const_type = decl.value.expr_type().unwrap().unwrap();

            write!(out, "{} const {} = ", type_to_c(&const_type, decl.scope()), decl.name)?;
            write_expr(out, &decl.value, globals)?;
            writeln!(out, ";")
        })
        .collect()
}

pub fn write_vars(out: &mut String, vars: &semantic::VarDecls) -> fmt::Result {
    vars.decls.iter()
        .map(|decl| {
            writeln!(out, "{} {};", type_to_c(&decl.decl_type, decl.scope()), decl.name)
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
            let decl_id = Identifier::from(&decl.name);
            let decl_type = decl.decl_type.clone();

            default_initialize(out, &TypedSymbol::new(decl_id, decl_type))
        })
        .collect()
}

pub fn write_record_decl(out: &mut String, record_decl: &semantic::RecordDecl) -> fmt::Result {
    assert!(record_decl.members.len() > 0, "structs must have at least one member");

    let qualified_id = identifier_to_c(&record_decl.qualified_name());
    if record_decl.kind == RecordKind::Class {
        writeln!(out, "struct {}: System_Internal_Object {{", qualified_id)?;
    } else {
        writeln!(out, "struct {} {{", qualified_id)?;
    }

    for member in record_decl.members.iter() {
        writeln!(out, "{} {};", type_to_c(&member.decl_type, record_decl.scope()), &member.name)?;
    }
    writeln!(out, "}};")?;
    writeln!(out)
}

pub fn write_function_decl(out: &mut String,
                           function: &semantic::FunctionDecl,
                           _globals: &mut ModuleGlobals) -> fmt::Result {
    let return_type_c = function.return_type.as_ref()
        .map(|return_type| type_to_c(return_type, function.scope()));

    let qualified_name = function.scope().qualify_local_name(&function.name);

    // never use C++ name mangling
    write!(out, "extern \"C\" ")?;

    write!(out, "{} ", return_type_c.clone().unwrap_or_else(|| "void".to_owned()))?;
    write!(out, "{} ", identifier_to_c(&qualified_name))?;

    write!(out, "({})", function.args.iter()
        .map(|arg_decl| {
            let c_arg_type_base = type_to_c(&arg_decl.decl_type, &arg_decl.scope());

            let c_type = match &arg_decl.modifier {
                None => c_arg_type_base,
                Some(FunctionArgModifier::Const) => format!("{} const", c_arg_type_base),
                Some(FunctionArgModifier::Var) |
                Some(FunctionArgModifier::Out) => format!("{}&", c_arg_type_base),
            };

            format!("{} {}", c_type, arg_decl.name)
        })
        .collect::<Vec<_>>()
        .join(", "))
}

pub fn write_function(out: &mut String,
                      function: &semantic::Function,
                      globals: &mut ModuleGlobals)
                      -> fmt::Result {
    write_function_decl(out, &function.decl, globals)?;

    writeln!(out, "{{")?;

    let mut all_local_vars = Vec::new();
    for local_decl in function.local_decls.iter() {
        match local_decl {
            FunctionLocalDecl::Consts(local_consts) => {
                write_consts(out, local_consts, globals)?;
            }

            FunctionLocalDecl::Vars(local_vars) => {
                write_vars(out, local_vars)?;
                default_initialize_vars(out, local_vars.decls.iter())?;
                all_local_vars.extend(local_vars.decls.iter());
            }

            FunctionLocalDecl::NestedFunction(_) =>
                unimplemented!("nested functions"),
        }
    }

    if function.decl.kind == FunctionKind::Constructor {
        //the actual return type is an Rc, but we need to pass the class type to sizeof
        let constructed_class_c_name = match function.decl.return_type.as_ref().unwrap() {
            Type::Class(name) => {
                identifier_to_c(&name)
            }
            _ => panic!("constructor must return a class type"),
        };

        writeln!(out, "result = ({}*)System_Internal_Rc_GetMem(sizeof(struct {}), \"{}\");",
                 constructed_class_c_name,
                 constructed_class_c_name,
                 function.decl.return_type.as_ref().expect("constructor must have return type"))?;
    }

    let rc_args: Vec<_> = function.decl.args.iter()
        .filter(|arg| {
            arg.modifier != Some(FunctionArgModifier::Out)
                && arg.decl_type.is_class()
        })
        .collect();

    // retain rc args to non-destructor functions
    // this is kind of a hack to make sure temporary values used as args get rc'd
    // destructors musn't change the ref count of their only arg, the dead object, or bad things
    // happen
    // it's safe to do this with a constructor because the object under construction exists only
    // in the result position
    if function.decl.kind == FunctionKind::Function {
        for arg in rc_args.iter() {
            writeln!(out, "System_Internal_Rc_Retain({});", arg.name)?;
        }
    }

    write_block(out, &function.block, globals)?;
    release_vars(out, all_local_vars.into_iter()
        .filter(|var| var.name != "result"))?;

    if function.decl.kind == FunctionKind::Function {
        //release args
        for arg in rc_args.iter() {
            writeln!(out, "System_Internal_Rc_Release({});", arg.name)?;
        }
    }

    let return_type_c = function.decl.return_type.as_ref()
        .map(|return_type| type_to_c(return_type, function.scope()));

    match return_type_c {
        Some(_) => writeln!(out, "return result;")?,
        None => (),
    }

    writeln!(out, "}}")?;
    writeln!(out)
}

pub fn write_impl_decl(out: &mut String,
                       impl_decl: &semantic::Implementation,
                       unit_name: Option<&Identifier>,
                       globals: &mut ModuleGlobals) -> fmt::Result {
    match impl_decl {
        Implementation::Function(func_impl) =>
            write_function(out, func_impl, globals),
        Implementation::Decl(decl) => {
            write_decl(out, decl, unit_name, globals)
        }
    }
}

pub fn write_decl(out: &mut String,
                  decl: &semantic::UnitDecl,
                  unit_name: Option<&Identifier>,
                  globals: &mut ModuleGlobals) -> fmt::Result {
    let qualify_name = |name: &str| {
        match &unit_name {
            Some(unit_name) => identifier_to_c(&unit_name.child(name)),
            None => identifier_to_c(&Identifier::from(name))
        }
    };

    match decl {
        UnitDecl::Function(ref func_decl) => {
            write_function_decl(out, func_decl, globals)?;
            writeln!(out, ";")
        }

        UnitDecl::Type(ref type_decl) =>
            match type_decl {
                node::TypeDecl::Record(record_decl) =>
                    write_record_decl(out, record_decl),

                node::TypeDecl::Enumeration(enum_decl) => {
                    let enum_name = qualify_name(&enum_decl.name);
                    // all enums are backed by u64 for now
                    writeln!(out, "using {} = System_UInt64;", enum_name)
                }

                node::TypeDecl::Set(set_decl) => {
                    let set_name = qualify_name(&set_decl.name);

                    // all sets are backed by u64
                    writeln!(out, "using {} = System_UInt64;", set_name)
                }

                node::TypeDecl::Alias { .. } => {
                    // these decls don't create any new types in the C++ output and can be ignored
                    Ok(())
                }
            }

        UnitDecl::Vars(ref vars_decl) =>
            write_vars(out, vars_decl),

        UnitDecl::Consts(ref consts_decl) =>
            write_consts(out, consts_decl, globals),
    }
}

fn write_static_init_impls<'a>(out: &mut String,
                               impls: &'a [semantic::Implementation],
                               ns: Option<&Identifier>) -> fmt::Result {
    let decls: Vec<_> = impls.iter()
        .filter_map(|impl_decl| match impl_decl {
            Implementation::Decl(decl) => Some(decl.clone()),
            _ => None,
        })
        .collect();

    write_static_init(out, decls.as_slice(), ns)
}

pub fn write_static_init<'a>(out: &mut String,
                             decls: &'a [semantic::UnitDecl],
                             namespace: Option<&Identifier>)
                             -> fmt::Result {
    let classes = decls.iter()
        .filter_map(|decl| {
            match decl {
                UnitDecl::Type(node::TypeDecl::Record(record))
                if record.kind == RecordKind::Class => {
                    Some(record)
                }

                _ => None,
            }
        });

    for class in classes {
        let qualified_name = match namespace {
            Some(ns) => ns.child(&class.name),
            None => Identifier::from(&class.name),
        };

        /* string is magically initialized earlier */
        if qualified_name.to_string() == "System.String" {
            continue;
        }

        let destructor = decls.iter()
            .filter_map(|decl| match decl {
                UnitDecl::Function(func_decl)
                if func_decl.is_destructor_of(&class)
                => Some(func_decl),
                _ => None
            })
            .next();

        let destructor_ptr = match destructor {
            Some(destructor) => {
                let dtor_qualified = destructor.scope().qualify_local_name(&destructor.name);
                format!("(System_Internal_Destructor)&{}", identifier_to_c(&dtor_qualified))
            }
            None => "nullptr".to_string(),
        };

        writeln!(out, "System_Internal_InitClass(\"{}\", {});", qualified_name, destructor_ptr)?;
    }

    Ok(())
}

pub fn write_c(module: &ProgramModule)
               -> Result<String, fmt::Error> {
    let mut globals = ModuleGlobals::new();

    let mut c_decls = String::new();

    for unit in module.units.iter() {
        writeln!(c_decls, "/* {} interface */", unit.name)?;

        let unit_name = Identifier::from(&unit.name);
        for decl in unit.interface.iter() {
            write_decl(&mut c_decls, decl, Some(&unit_name), &mut globals)?;
        }
    }

    for unit in module.units.iter() {
        writeln!(c_decls, "/* {} implementation */", unit.name)?;
        for imp_decl in unit.implementation.iter() {
            let unit_name = Identifier::from(&unit.name);
            write_impl_decl(&mut c_decls, imp_decl, Some(&unit_name), &mut globals)?;
        }
    }

    writeln!(c_decls, "/* program decls */")?;
    for impl_decl in module.program.decls.iter() {
        write_impl_decl(&mut c_decls, impl_decl, None, &mut globals)?;
    }

    let var_decls: Vec<_> = module.program.decls.iter()
        .filter_map(|decl| match decl {
            Implementation::Decl(UnitDecl::Vars(ref vars_decl)) => Some(vars_decl),
            _ => None
        })
        .flat_map(|decl_group| decl_group.decls.iter())
        .collect();

    /* write main function */
    let mut c_main = String::new();

    writeln!(c_main, "/* program block */")?;

    default_initialize_vars(&mut c_main, var_decls.iter().cloned())?;

    for unit in module.units.iter() {
        if let Some(init_block) = unit.initialization.as_ref() {
            write_block(&mut c_main, &init_block, &mut globals)?;
        }
    }

    write_block(&mut c_main, &module.program.program_block, &mut globals)?;
    release_vars(&mut c_main, var_decls)?;

    for unit in module.units.iter().rev() {
        if let Some(finalize_block) = unit.finalization.as_ref() {
            write_block(&mut c_main, &finalize_block, &mut globals)?;
        }
    }

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
        let unit_ns = Identifier::from(&unit.name);
        write_static_init(&mut output, &unit.interface, Some(&unit_ns))?;
        write_static_init_impls(&mut output, &unit.implementation, Some(&unit_ns))?;
    }
    write_static_init_impls(&mut output, &module.program.decls, None)?;

    output.write_str(&c_main)?;

    writeln!(output, "return 0;")?;
    writeln!(output, "}}")?;

    output.write_str(RT)?;

    Ok(output)
}
