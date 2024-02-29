use std::fmt::Debug;

use crate::compile::ByteCode;
use crate::runtime::RuntimeType;

#[derive(Clone)]
pub struct Function {
    pub parameters: Vec<(String, RuntimeType)>,
    pub body: ByteCode,
}

impl Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (name, r#type) in &self.parameters {
            write!(f, "{}: {}, ", name, r#type)?;
        }
        write!(f, "=> {}", "function body")
    }
}


impl Function {
    pub fn new(parameters: Vec<(String, RuntimeType)>, body: ByteCode) -> Self {
        Self {
            parameters,
            body,
        }
    }
}