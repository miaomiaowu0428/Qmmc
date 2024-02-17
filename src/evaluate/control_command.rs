use crate::evaluate::Value;

#[derive(Debug, PartialEq)]
pub enum  ControlCommand {
    Break,
    Continue,
    Return(Value),
}