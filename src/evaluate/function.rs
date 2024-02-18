use std::fmt::Display;
use std::rc::Rc;

use crate::analyze::syntax_tree::Expression;
use crate::evaluate::RuntimeScope;

#[derive(Debug, Clone)]
pub struct Function {
    pub parent_scope:Rc<RuntimeScope>,
    pub parameters: Vec<String>,
    pub body: Expression,
    pub declared_expression: Expression
}


impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fun({}) -> {}", self.parameters.join(", "), self.body)
    }
}

impl Function {
    pub fn new(parent_scope: Rc<RuntimeScope>, parameters: Vec<String>, body: Expression, declared_expression: Expression) -> Self {
        Self {
            parent_scope,
            parameters,
            body,
            declared_expression
        }
    }
}