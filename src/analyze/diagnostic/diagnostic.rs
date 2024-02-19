#![allow(dead_code)]

use std::cell::RefCell;
use std::rc::Rc;

use colored::Colorize;

use TokenType::{FloatPointToken, IntegerToken};

use crate::analyze::lex::token::{Token, TokenType};
use crate::analyze::syntax_tree::Expression;
use crate::evaluate::{Function, Type};

#[derive(Debug, Clone)]
pub struct DiagnosticBag {
    pub diagnostics: RefCell<Vec<Diagnostic>>,
}


impl DiagnosticBag {
    pub(crate) fn clear(&self) {
        self.diagnostics.borrow_mut().clear();
    }
}


impl DiagnosticBag {
    pub fn new() -> Self {
        Self {
            diagnostics: RefCell::new(Vec::new()),
        }
    }

    pub(crate) fn append(&self, p0: DiagnosticBag) {
        self.diagnostics.borrow_mut().append(&mut p0.diagnostics.borrow_mut());
    }

    pub(crate) fn report_invalid_literal(&self, token: Token) {
        let message = format!("Invalid literal '{}'", token.text.red());
        self.report(message);
    }

    pub(crate) fn report_undefined_function(&self, p0: &str) {
        let message = format!("Found no function named '{}'", p0.red());
        self.report(message);
    }

    pub(crate) fn report_undefined_variable(&self, name: &str) {
        let message = format!("Found no variable named '{}'", name.to_string().red());
        self.report(message);
    }
    pub(crate) fn report_immutable_variable(&self, token: Token, expression: Expression) {
        let message = format!("Assignment to immutable variable '{}' because it's declared with 'val'. consider change it to var", token.to_string().red());
        let line_num = expression.to_token_vec().first().unwrap().line_num;
        let column_num = expression.to_token_vec().first().unwrap().column_num;
        let message = format!("{message}\n> variable {token} declared here: ({line_num},{column_num}) {}", expression
            .to_token_vec().iter()
            .map(|t| format!("{} ", t.to_string()))
            .collect::<String>());
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


    pub(crate) fn report_argument_type_mismatch(&self, function_name: &str, parameter_name: &str, need: Type, found: Type) {
        let message = format!("parameter {} in function:{} need type {}, but {} is given", parameter_name.red(), function_name.blue(), need.to_string().green(), found.to_string().red());
        self.report(message);
    }
    pub(crate) fn report_argument_count_mismatch(&self, function_name: &String, need: usize, given: usize) {
        let message = format!("function {} need {} arguments, but {} is given", function_name.red(), need.to_string().green(), given.to_string().red());
        self.report(message);
    }

    pub fn report_unexpected_token(&self, tokens_in_the_same_line: Vec<&Token>, token: Token, expected: &Vec<TokenType>, ) {
        let line = tokens_in_the_same_line
            .iter()
            .map(|t| if t.column_num == token.column_num {
                t.to_string().red().to_string()
            } else {
                t.to_string().normal().to_string()
            })
            .collect::<Vec<String>>()
            .join(" ");
        let expected = expected
            .iter()
            .map(|t| format!("<{:?}>", t))
            .collect::<Vec<String>>()
            .join(", ");

        let message = format!(
            ">({},{}): {}\nUnexpected token '<{}>', expected '{}'",
            token.line_num,
            token.column_num,
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


    pub(crate) fn report_continue_function_call(&self, name: &String, fun: Rc<Function>) {
        let message = format!("cannot use continue in function {{ {name} }},which is declared by:\n {fun}");
        self.report(message);
    }
    pub(crate) fn report_break_function_call(&self, name: &String, fun: Rc<Function>) {
        let message = format!("cannot use break in function {{ {name} }},which is declared by:\n {fun}");
        self.report(message);
    }
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub message: String,
}
