#![allow(dead_code)]

use crate::analyze::syntax_tree::Expression;
use crate::evaluate::Type;
use crate::evaluate::value::Value;

#[derive(Debug, Clone)]
pub struct Variable {
    pub mutable: bool,
    pub value: Value,
    pub initialized: bool,
    pub declared_expression: Expression
}

impl Variable {
    pub fn new(mutable: bool, value: Value, initialized: bool, declared_expression: Expression) -> Self {
        Self {
            mutable,
            value,
            initialized,
            declared_expression
        }
    }


    pub fn init_as_mutable(value: Value, declared_expression: Expression) -> Self {
        Self::new(true, value, true, declared_expression)
    }

    pub fn init_as_immutable(value: Value, declared_expression: Expression) -> Self {
        Self::new(false, value, true, declared_expression)
    }

    pub fn uninit_mutable(declared_expression: Expression) -> Self {
        Self::new(true, Value::None, false, declared_expression)
    }
    pub fn uninit_immutable(declared_expression: Expression) -> Self {
        Self::new(false, Value::None, false, declared_expression)
    }

    pub fn r#type(&self) -> Type {
        self.value.r#type()
    }
}