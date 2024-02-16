#![allow(dead_code)]

use crate::analyze::syntax_tree::Expression;
use crate::evaluate::Type;
use crate::evaluate::value::Value;

#[derive(Debug, Clone)]
pub struct Variable {
    pub mutable: bool,
    pub value: Value,
    pub declared_expression: Expression
}

impl Variable {
    pub fn new(mutable: bool, value: Value, declared_expression: Expression) -> Self {
        Self {
            mutable,
            value,
            declared_expression
        }
    }
    pub fn new_mutable(value: Value, declared_expression: Expression) -> Self {
        Self::new(true, value, declared_expression)
    }
    pub fn new_immutable(value: Value, declared_expression: Expression) -> Self {
        Self::new(false, value, declared_expression)
    }

    pub fn r#type(&self) -> Type {
        self.value.r#type()
    }
}