use crate::compile::CheckedExpression;

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum RawType {
    None,
    I32,
    Bool,
    F32,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionType {
    pub param_types: Vec<RawType>,
    pub return_type: Box<RawType>,
}

#[derive(Debug, Clone)]
pub struct FunctionDeclare {
    pub _type: FunctionType,
    pub param_names: Vec<String>,
    pub body: CheckedExpression,
}
