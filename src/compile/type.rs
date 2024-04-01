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

impl RawType {
    pub fn same_as(&self, other: &RawType) -> bool {
        match (self, other) {
            (RawType::Unit, RawType::Unit) => true,
            (RawType::I32, RawType::I32) => true,
            (RawType::Bool, RawType::Bool) => true,
            (RawType::F32, RawType::F32) => true,
            (RawType::Char, RawType::Char) => true,
            (RawType::Byte, RawType::Byte) => true,
            (
                RawType::Array {
                    inner_type: a,
                    size: s1,
                },
                RawType::Array {
                    inner_type: b,
                    size: s2,
                },
            ) => a.same_as(b) && s1 == s2,
            (RawType::Pointer { inner_type: a }, RawType::Pointer { inner_type: b }) => {
                a.same_as(b)
            }
            (RawType::StringLiteral, RawType::StringLiteral) => true,
            _ => false,
        }
    }

    pub fn ptr_type_of(inner_type:RawType) -> RawType {
        RawType::Pointer {
            inner_type: Box::new(inner_type)
        }
    }
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
