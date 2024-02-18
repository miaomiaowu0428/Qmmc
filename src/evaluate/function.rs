use std::fmt::Display;

use crate::analyze::syntax_tree::Expression;

#[derive(Debug, Clone)]
pub struct Function {
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
    pub fn new(parameters: Vec<String>, body: Expression, declared_expression: Expression) -> Self {
        Self {
            parameters,
            body,
            declared_expression
        }
    }
}