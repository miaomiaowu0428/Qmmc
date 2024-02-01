use std::cell::RefCell;
use std::fmt::Display;
use crate::analyze::lex::token::Token;


#[derive(Debug, Clone)]
pub struct Line {
    pub tokens:RefCell<Vec<Token>>
}
impl Line {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens: RefCell::new(tokens),
        }
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