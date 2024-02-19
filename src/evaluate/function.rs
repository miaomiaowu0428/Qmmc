use std::fmt::Display;
use std::rc::Rc;

use crate::analyze::syntax_tree::{Expression, NameTypePair};
use crate::evaluate::RuntimeScope;

#[derive(Debug, Clone)]
pub struct Function {
    pub parent_scope: Rc<RuntimeScope>,
    pub parameters: Vec<NameTypePair>,
    pub body: Expression,
    pub declared_expression: Expression
}


impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fun({}) -> {}", self.parameters.iter().map(|p| p.to_string()).collect::<Vec<String>>().join(", "), self.body)
    }
}

impl Function {
    pub fn new(parent_scope: Rc<RuntimeScope>, parameters: Vec<NameTypePair>, body: Expression, declared_expression: Expression) -> Self {
        Self {
            parent_scope,
            parameters,
            body,
            declared_expression
        }
    }
}