#![allow(dead_code)]

use crate::analyze::lex::token::{Token, TokenType};
use colored::Colorize;
use std::cell::RefCell;
use TokenType::{FloatPointToken, IntegerToken};
use crate::analyze::lex::line::Line;
use crate::evaluate::Type;

pub struct DiagnosticBag {
    pub diagnostics: RefCell<Vec<Diagnostic>>,
}



impl DiagnosticBag {
    pub(crate) fn report_invalid_literal(&self, token: Token) {
        let message = format!("Invalid literal '{}'", token.text.red());
        self.report(message);
    }
}


impl DiagnosticBag {
    pub fn new() -> Self {
        Self {
            diagnostics: RefCell::new(Vec::new()),
        }
    }

    pub(crate) fn report_undefined_variable(&self, token: Token) {
        let message = format!("Found no variable named '{}'", token.text.red());
        self.report(message);
    }
    pub(crate) fn report_immutable_variable(&self, token: Token) {
        let message = format!("Assignment to immutable variable '{}' because it's declared with 'val'. consider change it to var", token.text.red());
        self.report(message);
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

    pub fn report_unexpected_token(&self, token: Token, expected: &Vec<TokenType>, line: Line, line_num: usize, pos: usize, ) {
        let expected = expected
            .iter()
            .map(|t| format!("<{:?}>", t))
            .collect::<Vec<String>>()
            .join(", ");
        let message = format!(
            ">({},{}): {}\nUnexpected token '<{}>', expected '{}'",
            line_num,
            pos,
            line,
            format!("{:?}", token.token_type).red(),
            expected.bright_green(),
        );
        self.report(message);
    }

    pub(crate) fn report_invalid_number(&self, token: Token) {
        let aim_type = match token.token_type {
            IntegerToken => "i32",
            FloatPointToken => "f32",
            _ => "None",
        };
        let msg = format!("cannot parse {} into {}", token.text.red(), aim_type);
        self.report(msg)
    }

    pub(crate) fn report_invalid_binary_op(&self, left_type: Type, op_token: Token, right_type: Type) {
        let msg = format!("operator {} is not defined for {} and {}",
                          op_token.text.red(),
                          left_type.to_string().bright_yellow(),
                          right_type.to_string().bright_yellow());
        self.report(msg);
    }

    pub(crate) fn report_invalid_unary_op(&self, op_token: Token, operand_type: Type) {
        let msg = format!("operator {} is not defined for {}",
                          op_token.text.red(),
                          operand_type.to_string().bright_yellow());
        self.report(msg);
    }

    pub(crate) fn report_unexpected_expression(&self, line: Line) {
        let msg = format!("unexpected expression '{}_'. consider add a semicolon",
                          format!("{}", line).red());
        self.report(msg);
    }
}

pub struct Diagnostic {
    pub message: String,
}
