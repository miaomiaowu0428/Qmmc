use std::fmt::Debug;
use std::rc::Rc;

use crate::runtime::{RuntimeScope, RuntimeType};
use crate::compile::ByteCode;

#[derive(Clone)]
pub struct Function {
    pub parent_scope: Rc<RuntimeScope>,
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
    pub fn new(parent_scope: Rc<RuntimeScope>, parameters: Vec<(String, RuntimeType)>, body: ByteCode) -> Self {
        Self {
            parent_scope,
            parameters,
            body,
        }
    }
}