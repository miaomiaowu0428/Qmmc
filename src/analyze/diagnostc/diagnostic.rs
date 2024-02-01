#![allow(dead_code)]

use std::cell::RefCell;
use colored::Colorize;
use crate::analyze::lex::token::{Token, TokenType};

pub struct DiagnosticBag {
    pub diagnostics: RefCell<Vec<Diagnostic>>,
}

impl DiagnosticBag {
    pub fn new() -> Self {
        Self {
            diagnostics: RefCell::new(Vec::new()),
        }
    }
    pub fn report(&self, message: String) {
        self.diagnostics.borrow_mut().push(Diagnostic { message });
    }
    pub fn is_empty(&self) -> bool {
        self.diagnostics.borrow().is_empty()
    }

    pub fn print(&self) {
        for diagnostic in self.diagnostics.borrow().iter() {
            println!("{}", diagnostic.message);
        }
    }

    pub fn report_bad_token(&self, token: Token) {
        let message = format!("Bad token '<{}>'", token.to_string().red());
        self.report(message);
    }

    pub fn report_unexpected_token(&self, token: Token, expected: Vec<TokenType>) {
        let expected = expected.iter().map(|t| format!("<{}>", t)).collect::<Vec<String>>().join(", ");
        let message = format!("Unexpected token '<{}>', expected '{}'",
                              token.to_string().red(),
                              expected.bright_green());
        self.report(message);
    }
}


pub struct Diagnostic {
    pub message: String,
}