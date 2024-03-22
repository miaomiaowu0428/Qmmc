use crate::compile::CheckedExpression;

#[derive(Debug, Clone, PartialEq)]
pub enum RawType {
    Unit,
    I32,
    Bool,
    F32,
    Char,
    Byte,
    Array { inner_type: Box<RawType>, size: i32 },
    Pointer { inner_type: Box<RawType> },
    StringLiteral,
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
