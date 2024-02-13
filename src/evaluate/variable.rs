#![allow(dead_code)]

use crate::evaluate::value::Value;

#[derive(Debug, Clone)]
pub struct Variable {
    pub mutable: bool,
    pub value: Value,
}

impl Variable {
    pub fn new(mutable: bool, value: Value) -> Self {
        Self {
            mutable,
            value,
        }
    }
    pub fn new_mutable(value: Value) -> Self {
        Self::new(true, value)
    }
    pub fn new_immutable(value: Value) -> Self {
        Self::new(false, value)
    }
}