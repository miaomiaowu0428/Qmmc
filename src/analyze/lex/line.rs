#![allow(dead_code)]

use crate::analyze::lex::token::Token;
use std::cell::RefCell;
use std::fmt::Display;

#[derive(Debug, Clone)]
pub struct Line {
    pub tokens: RefCell<Vec<Token>>,
}
impl Line {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens: RefCell::new(tokens),
        }
    }
    pub fn len(&self) -> usize {
        self.tokens.borrow().len()
    }
    pub fn get(&self, index: usize) -> Token {
        self.tokens.borrow().get(index).unwrap().clone()
    }
}

impl Display for Line {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut string = String::new();
        for token in self.tokens.borrow().iter() {
            string.push_str(&format!("{}, ", token));
        }
        write!(f, "{}", string)
    }
}
