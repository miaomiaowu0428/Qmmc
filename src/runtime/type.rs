#![allow(non_camel_case_types)]

use std::fmt::{Display, Formatter};

use RuntimeType::Unknown;

use crate::runtime::RuntimeType::Bool;
use crate::runtime::RuntimeType::F32;
use crate::runtime::RuntimeType::I32;

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum RuntimeType {
    Unknown,
    None,
    I32,
    Bool,
    F32,
}

impl From<&String> for RuntimeType {
    fn from(s: &String) -> Self {
        match s.as_str() {
            "I32" => I32,
            "Bool" => Bool,
            "F32" => F32,
            _ => Unknown,
        }
    }
}

impl Display for RuntimeType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let string = match self {
            Unknown => "Unknown",
            RuntimeType::None => "None",
            I32 => "I32",
            Bool => "Bool",
            F32 => "F32",
        };
        write!(f, "{}", string)
    }
}