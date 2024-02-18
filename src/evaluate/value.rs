use std::fmt::Display;

use crate::evaluate::r#type::Type;

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy)]
pub enum Value {
    None,
    i32(i32),
    bool(bool),
    f32(f32),
}


impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::None, Value::None) => true,
            (Value::i32(i1), Value::i32(i2)) => i1 == i2,
            (Value::bool(b1), Value::bool(b2)) => b1 == b2,
            (Value::f32(f1), Value::f32(f2)) => f1 == f2,
            _ => false,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::None => write!(f, "None"),
            Value::i32(i) => write!(f, "{}", i),
            Value::bool(b) => write!(f, "{}", b),
            Value::f32(fl) => write!(f, "{}", fl),
        }
    }
}


impl Value {
    pub fn r#type(&self) -> Type {
        match self {
            Value::None => Type::None,
            Value::i32(_) => Type::I32,
            Value::bool(_) => Type::Bool,
            Value::f32(_) => Type::F32,
        }
    }
    pub(crate) fn as_i32(&self) -> i32 {
        match self {
            Value::i32(i) => *i,
            _ => panic!("Value is not an i32"),
        }
    }
    pub(crate) fn as_f32(&self) -> f32 {
        match self {
            Value::f32(f) => *f,
            _ => panic!("Value is not an f32"),
        }
    }

    pub(crate) fn as_bool(&self) -> bool {
        match self {
            Value::bool(b) => *b,
            _ => panic!("Value is not a bool"),
        }
    }
}
