use std::cell::RefCell;
use Expression::{IdentifierExpression, LiteralExpression};
use TokenType::{FalseKeyword, IdentifierToken, IntegerToken, LeftParenthesisToken, RightBraceToken, RightParenthesisToken, TrueKeyword};
use crate::analyze::diagnostc::DiagnosticBag;
use crate::analyze::lex::SourceFile;
use crate::analyze::lex::token::{Token, TokenType};
use crate::analyze::lex::token::TokenType::FloatPointToken;
use crate::analyze::syntax_tree::expression::Expression;
use crate::analyze::syntax_tree::expression::Expression::{BinaryExpression, ParenthesizedExpression, UnaryExpression};

pub struct SyntaxTree {
    source_file: SourceFile,
    line_number: RefCell<usize>,
    pos: RefCell<usize>,
    pub(crate) diagnostics: DiagnosticBag,
}

impl SyntaxTree {
    pub fn new(source_file: SourceFile) -> Self {
        Self {
            source_file,
            line_number: RefCell::from(0),
            pos: RefCell::from(0),
            diagnostics: DiagnosticBag::new(),
        }
    }

    pub fn parse_file(&self) -> Vec<Expression> {
        let mut res = Vec::new();
        loop {
            if self.line_number.borrow().clone() >= self.source_file.lines.len() {
                break;
            }
            res.push(self.parse_line());
            self.move_next_line();
        }
        res
    }


    pub fn parse_line(&self) -> Expression {
        *self.pos.borrow_mut() = 0;
        let expr = self.parse_expression();
        self.match_and_move(
            |t| t.token_type == TokenType::SemicolonToken || t.token_type == RightBraceToken,
            vec![TokenType::SemicolonToken, RightBraceToken]
        );
        expr
    }
    pub fn parse_expression(&self) -> Expression {
        let expr = self.parse_operator_expression(0);
        // self.move_next();

        expr
    }

    fn parse_operator_expression(&self, parent_priority: i32) -> Expression {
        let mut left;

        left = self.parse_unary_expression(parent_priority);

        loop {
            let binary_priority = self.current().token_type.get_binary_priority();
            if binary_priority == 0 || binary_priority <= parent_priority {
                break;
            }

            let op = self.current();
            self.move_next();
            let right = self.parse_operator_expression(binary_priority);
            left = BinaryExpression {
                left: Box::new(left),
                operator_token: op,
                right: Box::new(right),
            }
        }
        left
    }

    fn parse_unary_expression(&self, parent_priority: i32) -> Expression {
        let left;

        let unary_priority = self.current().token_type.get_unary_priority();
        if unary_priority == 0 || unary_priority < parent_priority {
            left = self.parse_literal_expression()
        } else {
            let op = self.current();
            self.move_next();
            let operand = self.parse_operator_expression(unary_priority);
            left = UnaryExpression {
                operator_token: op,
                operand: Box::new(operand),
            }
        }
        left
    }


    fn parse_literal_expression(&self) -> Expression {
        match self.current().token_type {
            IntegerToken
            | FloatPointToken
            | TrueKeyword
            | FalseKeyword
            => {
                let res = LiteralExpression { literal_token: self.current() };
                self.move_next();
                res
            }
            IdentifierToken => {
                let res = IdentifierExpression { identifier_token: self.current() };
                self.move_next();
                res
            }
            LeftParenthesisToken => {
                let left_p = self.current();
                self.move_next();
                let expr = self.parse_expression();
                self.match_and_move(
                    |t| t.token_type == RightParenthesisToken,
                    vec![RightParenthesisToken]
                );
                let right_p = self.current();
                ParenthesizedExpression {
                    left_p,
                    expression: Box::new(expr),
                    right_p,
                }
            }
            _ => {
                let res = LiteralExpression { literal_token: self.current() };
                self.match_and_move(
                    |t| t.token_type == TokenType::SemicolonToken || t.token_type == RightBraceToken,
                    vec![TokenType::SemicolonToken, RightBraceToken]
                );
                res
            }
        }
    }

    fn match_and_move<F>(&self, predict: F, expected: Vec<TokenType>) -> bool
    where F: Fn(Token) -> bool {
        if predict(self.current()) {
            self.move_next();
            true
        } else {
            self.diagnostics.report_unexpected_token(self.current(), expected);
            self.move_next();
            false
        }
    }


    fn peek(&self, offset: usize) -> Token {
        let line_num = self.line_num();
        let pos = self.pos();

        if pos + offset >= self.line_len() {
            self.source_file.lines
                [line_num].tokens.borrow()
                [self.line_len() - 1].clone()
        } else {
            self.source_file.lines
                [line_num].tokens.borrow()
                [pos + offset].clone()
        }
    }
    fn current(&self) -> Token {
        self.peek(0)
    }

    fn line_num(&self) -> usize {
        self.line_number.borrow().clone()
    }
    fn line_len(&self) -> usize {
        self.source_file.lines
            [self.line_num()].tokens.borrow().len()
    }
    fn pos(&self) -> usize {
        self.pos.borrow().clone()
    }
    fn move_next(&self) {
        *self.pos.borrow_mut() += 1;
    }

    fn move_next_line(&self) {
        *self.line_number.borrow_mut() += 1;
        *self.pos.borrow_mut() = 0;
    }
}

