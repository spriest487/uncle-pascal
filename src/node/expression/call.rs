use node::{
    Context,
    Expression,
    Identifier,
};

#[derive(Clone, Debug, PartialEq)]
pub enum FunctionCall<TContext>
    where TContext: Context,
{
    Function {
        target: Box<Expression<TContext>>,

        args: Vec<Expression<TContext>>,
    },
    Method {
        interface_id: Identifier,
        func_name: String,
        for_type: TContext::Type,

        args: Vec<Expression<TContext>>,
    },
    Extension {
        self_expr: Box<Expression<TContext>>,
        func_name: String,
        for_type: TContext::Type,

        args: Vec<Expression<TContext>>,
    }
}

impl<TContext> FunctionCall<TContext>
    where TContext: Context
{
    pub fn args(&self) -> &[Expression<TContext>] {
        match self {
            FunctionCall::Function { args, .. } => args,
            FunctionCall::Method { args, .. } => args,
            FunctionCall::Extension { args, .. } => args,
        }
    }

    pub fn unwrap_function(self) -> (Expression<TContext>, Vec<Expression<TContext>>) {
        match self {
            FunctionCall::Function { target, args } => (*target, args),
            FunctionCall::Extension { .. } => panic!("called unwrap_function on extension call"),
            FunctionCall::Method { interface_id, func_name, .. } =>
                panic!("called unwrap_function on method call {}.{}", interface_id, func_name)
        }
    }
}
