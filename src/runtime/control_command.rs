use crate::runtime::Value;

#[derive(Debug, PartialEq, Clone)]
pub enum ControlCommand {
    Break,
    Continue,
    Return(Value),
}