use {pas_syn::ast, pas_typecheck as pas_ty};

#[derive(Debug, Clone)]
pub enum Type {
    None,
    I32,
}

fn translate_type(ty: &pas_ty::Type) -> Type {
    match ty {
        pas_typecheck::Type::None => Type::None,
        pas_typecheck::Type::Integer => Type::I32,
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Local(usize),
    LiteralI32(i32),
}

#[derive(Debug, Clone)]
pub enum Instruction {
    ScopeAlloc(usize, Type),
    Set(usize, Value),
}

pub fn translate_expr(
    expr: &pas_ty::ast::ExpressionNode,
    _next_id: &mut usize,
    _prologue: &mut Vec<Instruction>,
) -> Value {
    match expr.expr.as_ref() {
        ast::Expression::LiteralInt(i) => Value::LiteralI32(i.as_i32().unwrap()),
        _ => unimplemented!("expression IR for {}", expr),
    }
}

pub fn translate_stmt(stmt: &pas_ty::ast::Statement, next_id: &mut usize) -> Vec<Instruction> {
    let mut instructions = Vec::new();

    match stmt {
        ast::Statement::LetBinding(binding) => {
            let val_ty = translate_type(&binding.val_ty);

            let val = translate_expr(&binding.val, next_id, &mut instructions);

            let id = *next_id;
            *next_id += 1;
            instructions.push(Instruction::ScopeAlloc(id, val_ty));
            instructions.push(Instruction::Set(id, val));
        }
    }

    instructions
}

pub fn translate(unit: &pas_ty::ast::Unit) -> Vec<Instruction> {
    let mut instructions = Vec::new();

    let mut next_id = 0;
    for stmt in &unit.init {
        instructions.extend(translate_stmt(stmt, &mut next_id));
    }

    instructions
}
