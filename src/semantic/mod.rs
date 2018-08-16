use std::fmt;

use node;
use types::*;
use syntax;

pub mod scope;

pub use self::scope::*;

#[derive(Clone, Debug)]
pub enum SemanticError {
    UnknownType(node::Identifier),
    UnknownSymbol(node::Identifier),
}

impl fmt::Display for SemanticError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &SemanticError::UnknownType(ref missing_type) => {
                write!(f, "type not found: {}", missing_type)
            }

            &SemanticError::UnknownSymbol(ref missing_sym) => {
                write!(f, "symbol not found: {}", missing_sym)
            }
        }
    }
}

type Vars = node::Vars<Symbol>;
type VarDecl = node::VarDecl<Symbol>;

impl Into<Symbol> for VarDecl {
    fn into(self) -> Symbol {
        Symbol::new(node::Identifier::parse(&self.name), self.decl_type)
    }
}

impl Vars {
    fn annotate(vars: &syntax::Vars, scope: &Scope) -> Result<Self, SemanticError> {
        let decls = vars.decls.iter()
            .map(|v| -> Result<VarDecl, SemanticError> {
                let var_type = scope.get_type(&v.decl_type)
                    .cloned()
                    .ok_or_else(|| SemanticError::UnknownType(v.decl_type.clone()))?;

                Ok(VarDecl {
                    name: v.name.clone(),
                    decl_type: var_type,
                })
            })
            .collect::<Result<_, _>>()?;

        Ok(Self {
            decls
        })
    }
}

type Expression = node::Expression<Symbol>;

impl Expression {
    fn annotate(expr: &syntax::Expression, scope: &Scope) -> Result<Self, SemanticError> {
        match expr {
            &node::Expression::Identifier(ref name) => {
                scope.get_symbol(name)
                    .map(|symbol| {
                        Expression::identifier(symbol)
                    })
                    .ok_or_else(|| SemanticError::UnknownSymbol(name.clone()))
            }

            &node::Expression::Block(ref block) => {
                Ok(Expression::block(Block::annotate(block, scope)?))
            }

            &node::Expression::LiteralString(ref s) => {
                Ok(Expression::literal_string(s))
            }

            &node::Expression::LiteralInteger(i) => {
                Ok(Expression::literal_int(i))
            }

            &node::Expression::If { ref condition, ref then_branch, ref else_branch } => {
                let cond_expr = Expression::annotate(condition, scope)?;
                let then_expr = Expression::annotate(then_branch, scope)?;
                let else_expr = match else_branch {
                    &None => None,
                    &Some(ref expr) => Some(Expression::annotate(expr.as_ref(), scope)?),
                };

                Ok(Expression::if_then_else(cond_expr, then_expr, else_expr))
            }

            &node::Expression::BinaryOperator { ref lhs, ref op, ref rhs } => {
                Ok(Expression::binary_op(
                    Expression::annotate(lhs, scope)?,
                    op.clone(),
                    Expression::annotate(rhs, scope)?,
                ))
            }

            &node::Expression::FunctionCall { ref target, ref args } => {
                let typed_args = args.iter()
                    .map(|arg| Expression::annotate(arg, scope))
                    .collect::<Result<Vec<_>, _>>()?;

                let target_symbol = scope.get_symbol(target)
                    .ok_or_else(|| SemanticError::UnknownSymbol(target.clone()))?;

                Ok(Expression::function_call(target_symbol, typed_args))
            }
        }
    }
}

type Block = node::Block<Symbol>;

impl Block {
    fn annotate(block: &syntax::Block, scope: &Scope) -> Result<Self, SemanticError> {
        let statements = block.statements.iter()
            .map(|statement| {
                Expression::annotate(statement, scope)
            })
            .collect::<Result<_, _>>()?;

        Ok(Self {
            statements,
        })
    }
}

type Function = node::Function<Symbol>;

impl Function {
    fn annotate(function: &syntax::Function, scope: &Scope) -> Result<Self, SemanticError> {
        let return_type = scope.get_type(&function.return_type)
            .cloned()
            .ok_or_else(|| SemanticError::UnknownType(function.return_type.clone()))?;

        let local_vars = Vars::annotate(&function.local_vars, &scope)?;

        let result_sym = Symbol::new(node::Identifier::parse("result"), return_type.clone());

        let args = Vars::annotate(&function.args, scope)?;

        let local_scope = scope.clone()
            .with_symbols(args.decls.clone())
            .with_symbols(local_vars.decls.clone())
            .with_symbol(result_sym);

        let body = Block::annotate(&function.body, &local_scope)?;

        Ok(Function {
            name: function.name.clone(),
            return_type,
            local_vars,
            args,
            body,
        })
    }

    fn signature_type(&self) -> DeclaredType {
        let sig = FunctionSignature {
            decl_type: self.return_type.clone(),
            name: self.name.clone(),
            args_types: self.args.decls.iter().map(|arg| arg.decl_type.clone()).collect(),
        };

        DeclaredType::Function(Box::from(sig))
    }
}

type RecordDecl = node::RecordDecl<Symbol>;

impl RecordDecl {
    fn annotate(decl: &syntax::RecordDecl) -> Result<Self, SemanticError> {
        unimplemented!()
    }
}

pub type Program = node::Program<Symbol>;

impl Program {
    pub fn annotate(program: &syntax::Program) -> Result<Self, SemanticError> {
        let global_scope = Scope::default();

        let vars = Vars::annotate(&program.vars, &global_scope)?;

        let functions = program.functions.iter()
            .map(|f| Function::annotate(f, &global_scope))
            .collect::<Result<Vec<_>, _>>()?;

        let program_scope = global_scope
            .with_symbols(vars.decls.clone())
            .with_symbols(functions.iter().cloned().map(|f| {
                let identifier = node::Identifier::parse(&f.name);
                let sig_type = f.signature_type();

                Symbol::new(identifier, sig_type)
            }));

        let program_block = Block::annotate(&program.program_block, &program_scope)?;

        let type_decls = program.type_decls.iter()
            .map(|decl| RecordDecl::annotate(decl))
            .collect::<Result<_, _>>()?;

        Ok(Program {
            name: program.name.clone(),
            uses: program.uses.clone(),
            vars,
            functions,
            type_decls,
            program_block,
        })
    }
}
