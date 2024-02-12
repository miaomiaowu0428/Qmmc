#![allow(dead_code)]


use TokenType::{LeftBraceToken, RightBraceToken};
use crate::analyze::lex::line::Line;
use crate::analyze::lex::TokenType;


pub struct SourceFile {
    pub lines: Vec<Line>,
}

impl SourceFile {
    pub fn len(&self) -> usize {
        self.lines.len()
    }
    pub fn get(&self, index: usize) -> Option<&Line> {
        self.lines.get(index)
    }
    pub fn print(&self) {
        for i in 0..self.lines.len() {
            println!("{: >4}:{}{}", i, match self.get(i).unwrap().tokens.borrow()[0].token_type {
                LeftBraceToken => "",
                RightBraceToken => "",
                _ => "  ",
            }, self.lines[i]);
        }
    }
}

impl From<Vec<Line>> for SourceFile {
    fn from(lines: Vec<Line>) -> Self {
        for line_num in 0..lines.len() {
            let line = &lines[line_num];
            for token in line.tokens.borrow().iter() {
                *token.line_num.borrow_mut() = line_num;
            }
        }
        Self { lines }
    }
}
