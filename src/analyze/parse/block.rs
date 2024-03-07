use std::cell::RefCell;
use std::fmt::{Debug, Display};

use crate::analyze::lex::Token;
use crate::analyze::parse::Expression;

#[derive(Debug, Clone)]
pub struct Block {
    pub(crate) expressions: RefCell<Vec<Expression>>,
}

impl Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Ok(for expr in self.expressions.borrow().iter() {
            write!(f, "{}", expr)?;
        })
    }
}


impl Block {
    pub fn new() -> Self {
        Self {
            expressions: RefCell::new(Vec::new()),
        }
    }

    pub fn add_expression(&self, expression: Expression) {
        self.expressions.borrow_mut().push(expression);
    }

    pub fn format_with_indent(&self, f: &mut std::fmt::Formatter<'_>, indent: i32) -> std::fmt::Result {
        Ok(for expr in self.expressions.borrow().iter() {
            expr.format_with_indent(f, indent + 1)?;
            writeln!(f)?;
        })
    }

    pub fn to_token_vec(&self) -> Vec<Token> {
        let mut tokens = Vec::new();
        for expr in self.expressions.borrow().iter() {
            tokens.append(&mut expr.to_token_vec());
        }
        tokens
    }
}