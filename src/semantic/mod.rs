use std::fmt;

use node;
use types::*;
use syntax;

pub mod scope;

pub use self::scope::*;

#[derive(Clone, Debug)]
pub enum SemanticError {
    UnknownType(node::Identifier),
}

impl fmt::Display for SemanticError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &SemanticError::UnknownType(ref missing_type) => {
                write!(f, "type not found: {}", missing_type)
            }
        }
    }
}

type Vars = node::Vars<Symbol>;
type VarDecl = node::VarDecl<Symbol>;

impl Vars {
    fn annotate(vars: &syntax::Vars, scope: &Scope) -> Result<Self, SemanticError> {
        let decls = vars.decls.iter()
            .map(|v| -> Result<VarDecl, SemanticError> {
                let var_type = scope.find_type(&v.decl_type)
                    .cloned()
                    .ok_or_else(|| SemanticError::UnknownType(v.decl_type.clone()))?;

                Ok(VarDecl {
                    name: v.name.clone(),
                    decl_type: var_type
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
        unimplemented!()
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
        let return_type = scope.find_type(&function.return_type)
            .cloned()
            .ok_or_else(|| SemanticError::UnknownType(function.return_type.clone()))?;

        let local_vars = Vars::annotate(&function.local_vars, &scope)?;

        let mut local_scope = scope.clone();
        for local_var in local_vars.decls.iter() {
            let name = node::Identifier::parse(&local_var.name);
            local_scope = local_scope.with_symbol(name, local_var.decl_type.clone());
        }

        let body = Block::annotate(&function.body, &scope)?;

        Ok(Function {
            name: function.name.clone(),
            return_type,
            local_vars,
            body,
        })
    }
}

#[derive(Clone, Debug)]
pub struct Program {
    vars: Vars,
    functions: Vec<Function>,
}

impl Program {
    pub fn annotate(program: &syntax::Program) -> Result<Self, SemanticError> {
        let global_scope = Scope::default();

        let vars = Vars::annotate(&program.vars, &global_scope)?;

        let functions = program.functions.iter()
            .map(|f| Function::annotate(f, &global_scope))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Program {
            vars,
            functions,
        })
    }
}
