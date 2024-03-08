#![allow(dead_code)]

use crate::runtime::value::Value;
use crate::runtime::RuntimeType;

#[derive(Debug, Clone)]
pub struct Variable {
    pub value: Value,
}

impl Variable {
    pub fn new(value: Value) -> Self {
        Self { value }
    }

    pub fn r#type(&self) -> RuntimeType {
        self.value.r#type()
    }
}
