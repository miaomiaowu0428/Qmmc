use std::cell::RefCell;
use std::fmt::Debug;

use crate::analyze::lex::Token;
use crate::analyze::syntax_tree::Expression;
use crate::evaluate::Type;

#[derive(Debug, Clone)]
pub struct Block {
    pub(crate) expressions: RefCell<Vec<Expression>>,
}


impl Block {
    pub fn new() -> Self {
        Self {
            expressions: RefCell::new(Vec::new()),
        }
    }

    pub fn r#type(&self) -> Type {
        match self.expressions.borrow().last() {
            Some(e) => e.r#type(),
            None => Type::None,
        }
    }

    pub fn add_expression(&self, expression: Expression) {
        self.expressions.borrow_mut().push(expression);
    }

    pub fn print_as_line(&self, indent: i32) {
        for expr in self.expressions.borrow().iter() {
            expr.print_as_line(indent + 1);
            println!()
        }
    }

    pub fn to_token_vec(&self) -> Vec<Token> {
        let mut tokens = Vec::new();
        for expr in self.expressions.borrow().iter() {
            tokens.append(&mut expr.to_token_vec());
        }
        tokens
    }
}