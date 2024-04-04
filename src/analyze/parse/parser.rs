#![allow(dead_code)]

use colored::Colorize;
use std::cell::RefCell;

use Expression::LiteralExpression;
use Expression::VarDeclarationExpression;
use Expression::{AssignmentExpression, Type};
use Expression::{BreakExpression, IdentifierExpression, LoopExpression};
use TokenType::LeftBraceToken;
use TokenType::LeftParenthesisToken;
use TokenType::RightBraceToken;
use TokenType::RightParenthesisToken;
use TokenType::TrueKeyword;
use TokenType::ValKeyword;
use TokenType::VarKeyword;
use TokenType::WhileKeyword;
use TokenType::{
    ArrowToken, BreakKeyword, ColonToken, CommaToken, ContinueToken, FalseKeyword, FunKeyword,
    ReturnKeyword,
};
use TokenType::{CharToken, IfKeyword};
use TokenType::{EndLineToken, IntegerToken};
use TokenType::{IdentifierToken, LiteralStringToken};

use crate::analyze::diagnostic::DiagnosticBag;
use crate::analyze::lex::token::Token;
use crate::analyze::lex::token::TokenType;
use crate::analyze::lex::token::TokenType::FloatPointToken;
use crate::analyze::lex::TokenType::LoopKeyword;
use crate::analyze::lex::TokenType::{AmpersandToken, EndOfFileToken, EqualsToken, StarToken};
use crate::analyze::parse::block::Block;
use crate::analyze::parse::expression::Expression::BinaryExpression;
use crate::analyze::parse::expression::Expression::ParenthesizedExpression;
use crate::analyze::parse::expression::Expression::UnaryExpression;
use crate::analyze::parse::expression::{Expression, IdentifierTypePair};
use crate::analyze::parse::Expression::{
    BracketedExpression, ConditionalBranchExpression, ContinueExpression, FunctionCallExpression,
    FunctionDeclarationExpression, FunctionTypeExpression, Statement,
};

pub struct Parser {
    tokens: Vec<Token>,
    pos: RefCell<usize>,
    pub diagnostics: DiagnosticBag,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            pos: RefCell::from(0),
            diagnostics: DiagnosticBag::new(),
        }
    }
    pub fn parse(&self) -> Vec<Expression> {
        let mut expressions = Vec::new();
        while self.current().token_type != TokenType::EndOfFileToken {
            expressions.push(self.parse_expression());
        }
        expressions
    }

    fn parse_expression(&self) -> Expression {
        while self.current().token_type == EndLineToken {
            self.move_next();
        }

        let expr = match self.current().token_type {
            ValKeyword | VarKeyword => self.parse_declaration_expression(),
            IfKeyword => self.parse_conditional_branch_expression(),
            WhileKeyword => self.parse_while_expression(),
            LoopKeyword => self.parse_loop_expression(),
            BreakKeyword => BreakExpression {
                break_token: self.move_next(),
            },
            ContinueToken => ContinueExpression {
                continue_token: self.move_next(),
            },
            FunKeyword => self.parse_function_declaration(),
            ReturnKeyword => self.parse_return_expression(),
            _ => self.parse_operator_expression(0),
        };

        let expr = if self.current().token_type == EqualsToken {
            self.parse_assignment_expression(expr)
        } else {
            expr
        };

        let res = match self.current().token_type {
            TokenType::SemicolonToken => Statement {
                expression: Box::new(expr),
                semicolon: self.move_next(),
            },
            _ => expr,
        };

        while self.current().token_type == EndLineToken {
            self.move_next();
        }

        res
    }

    fn parse_function_declaration(&self) -> Expression {
        let fun_token = self.move_next(); // fun keyword

        let identifier_token =
            self.match_token(|t| t.token_type == IdentifierToken, vec![IdentifierToken]); // function name

        // function parameters
        let left_p = self.match_token(
            |t| t.token_type == LeftParenthesisToken,
            vec![LeftParenthesisToken],
        );
        let parameters = self.parse_function_parameters();
        let right_p = self.match_token(
            |t| t.token_type == RightParenthesisToken,
            vec![RightParenthesisToken],
        );

        // return type
        let (arrow_token, res_type_description) = if self.current().token_type == ArrowToken {
            let arrow_token = self.move_next();
            (arrow_token, Box::from(self.parse_type_description()))
        } else {
            (
                Token::new(
                    ArrowToken,
                    "->".to_string(),
                    self.current().line_num,
                    self.current().column_num,
                ),
                Box::from(Type { tokens: Vec::new() }),
            )
        };

        // function body
        let body = self.parse_block();
        FunctionDeclarationExpression {
            fun_token,
            identifier_token,
            left_p,
            parameters,
            right_p,
            arrow_token,
            res_type_description,
            body: Box::from(body),
        }
    }
    fn parse_type_description(&self) -> Expression {
        let mut tokens = Vec::new();
        while self.current().token_type != LeftBraceToken
            && self.current().token_type != RightParenthesisToken
            && self.current().token_type != CommaToken
            && self.current().token_type != EqualsToken
            && self.current().token_type != EndOfFileToken
        {
            if self.current().token_type == IdentifierToken
                || self.current().token_type == StarToken
                || self.current().token_type == AmpersandToken
            {
                tokens.push(self.move_next());
            } else {
                self.diagnostics.report(format!(
                    "you cannot use {} in type description",
                    format!("{}", self.current()).red()
                ));
                self.move_next();
            }
        }
        Type { tokens }
    }

    fn parse_return_expression(&self) -> Expression {
        let return_token = self.move_next();
        let expression = self.parse_expression();
        Expression::ReturnExpression {
            return_token,
            expression: Box::new(expression),
        }
    }

    fn parse_function_call(&self) -> Expression {
        let identifier_token = self.move_next();
        let left_p = self.match_token(
            |t| t.token_type == LeftParenthesisToken,
            vec![LeftParenthesisToken],
        );
        let arguments = self.parse_function_arguments();
        let right_p = self.match_token(
            |t| t.token_type == RightParenthesisToken,
            vec![RightParenthesisToken],
        );
        FunctionCallExpression {
            identifier_token,
            left_p,
            arguments,
            right_p,
        }
    }

    fn parse_function_arguments(&self) -> Vec<Expression> {
        let mut arguments = Vec::new();
        while self.current().token_type != RightParenthesisToken {
            arguments.push(self.parse_expression());
            if self.current().token_type == RightParenthesisToken {
                break;
            }
            self.match_token(|t| t.token_type == CommaToken, vec![CommaToken]);
        }
        arguments
    }
    fn parse_name_type_pair(&self) -> IdentifierTypePair {
        let name = self.match_token(|t| t.token_type == IdentifierToken, vec![IdentifierToken]);
        let colon = self.match_token(|t| t.token_type == ColonToken, vec![ColonToken]);
        let type_description = self.parse_type_description();
        IdentifierTypePair {
            name,
            colon,
            type_description,
        }
    }
    fn parse_function_parameters(&self) -> Vec<IdentifierTypePair> {
        let mut parameters = Vec::new();
        while self.current().token_type != RightParenthesisToken {
            parameters.push(self.parse_name_type_pair());
            if self.current().token_type == RightParenthesisToken {
                break;
            }
            self.match_token(|t| t.token_type == CommaToken, vec![CommaToken]);
        }
        parameters
    }

    fn parse_function_parameter_types(&self) -> Vec<Expression> {
        let mut parameters = Vec::new();
        while self.current().token_type != RightParenthesisToken {
            parameters.push(self.parse_type_description());
            if self.current().token_type == RightParenthesisToken {
                break;
            }
            self.match_token(|t| t.token_type == CommaToken, vec![CommaToken]);
        }
        parameters
    }

    fn parse_loop_expression(&self) -> Expression {
        let loop_token = self.move_next();
        let body = self.parse_block();
        LoopExpression {
            loop_token,
            body: Box::new(body),
        }
    }

    fn parse_block(&self) -> Expression {
        let lb = self.move_next();
        let block = Block::new();
        while self.current().token_type != RightBraceToken
            && self.current().token_type != EndOfFileToken
        {
            block.expressions.borrow_mut().push(self.parse_expression());
        }
        let rb = self.match_token(|t| t.token_type == RightBraceToken, vec![RightBraceToken]);
        BracketedExpression {
            left_b: lb,
            block,
            right_b: rb,
        }
    }

    fn parse_while_expression(&self) -> Expression {
        let while_token = self.move_next();
        let condition_expr = self.parse_operator_expression(0);
        let body_expr = self.parse_block();
        Expression::WhileExpression {
            while_token,
            condition: Box::new(condition_expr),
            body: Box::new(body_expr),
        }
    }

    fn parse_if_expr_or_block(&self) -> Expression {
        if self.current().token_type == LeftBraceToken {
            self.parse_block()
        } else {
            self.parse_conditional_branch_expression()
        }
    }

    fn parse_conditional_branch_expression(&self) -> Expression {
        let if_token = self.match_token(|t| t.token_type == IfKeyword, vec![IfKeyword]);
        // let if_token = self.move_next();
        let condition_expr = self.parse_operator_expression(0);
        let true_block = self.parse_block();
        let if_expr = Box::new(Expression::IfExpression {
            if_token,
            condition: Box::new(condition_expr),
            then_block: Box::new(true_block),
        });

        let mut else_if_blocks = Vec::new();
        while self.current().token_type == TokenType::ElseKeyword
            && self.peek(1).token_type == TokenType::IfKeyword
        {
            else_if_blocks.push(self.parse_else_if_expr());
        }

        let else_expr = if self.current().token_type == TokenType::ElseKeyword {
            let else_token = self.move_next();
            let else_block = self.parse_block();
            Some(Box::new(Expression::ElseExpression {
                else_token,
                block: Box::new(else_block),
            }))
        } else {
            None
        };

        ConditionalBranchExpression {
            if_expr,
            else_if_blocks,
            else_expr,
        }
    }
    fn parse_else_if_expr(&self) -> Expression {
        let else_token = self.match_token(
            |t| t.token_type == TokenType::ElseKeyword,
            vec![TokenType::ElseKeyword],
        );
        let if_token = self.match_token(
            |t| t.token_type == TokenType::IfKeyword,
            vec![TokenType::IfKeyword],
        );
        let condition_expr = self.parse_operator_expression(0);
        let body_expr = self.parse_block();
        Expression::ElseIfExpression {
            else_token,
            if_token,
            condition: Box::new(condition_expr),
            then_block: Box::new(body_expr),
        }
    }

    fn parse_assignment_expression(&self, expr: Expression) -> Expression {
        AssignmentExpression {
            aim_expr: Box::new(expr),
            equals_token: self.match_token(|t| t.token_type == EqualsToken, vec![EqualsToken]),
            expression: Box::new(self.parse_expression()),
        }
    }

    fn parse_declaration_expression(&self) -> Expression {
        let declaration_token = self.move_next();
        let identifier_token =
            self.match_token(|t| t.token_type == IdentifierToken, vec![IdentifierToken]);

        if self.current().token_type == EqualsToken {
            let equals_token = Some(self.move_next());
            let expression = Some(Box::new(self.parse_expression()));
            VarDeclarationExpression {
                declaration_token,
                identifier_token,
                equals_token,
                init_expr: expression,
            }
        } else {
            VarDeclarationExpression {
                declaration_token,
                identifier_token,
                equals_token: None,
                init_expr: None,
            }
        }
    }

    fn parse_operator_expression(&self, parent_priority: i32) -> Expression {
        let mut left = self.parse_unary_expression(parent_priority);

        loop {
            if self.current().token_type == EndLineToken {
                break;
            }

            let binary_priority = self.current().token_type.get_binary_priority();
            if binary_priority == 0 || binary_priority <= parent_priority {
                break;
            }

            let op = self.current();
            self.move_next();
            let right = self.parse_operator_expression(parent_priority);
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
            let op = self.move_next();
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
            IntegerToken | FloatPointToken | CharToken | TrueKeyword | FalseKeyword
            | LiteralStringToken => LiteralExpression {
                literal_token: self.move_next(),
            },
            IdentifierToken if self.peek(1).token_type == LeftParenthesisToken => {
                self.parse_function_call()
            }
            IdentifierToken => IdentifierExpression {
                identifier_token: self.move_next(),
            },
            LeftParenthesisToken => {
                let left_p = self.move_next();
                let expr = self.parse_operator_expression(0);

                let right_p = self.match_token(
                    |t| t.token_type == RightParenthesisToken,
                    vec![RightParenthesisToken],
                );
                ParenthesizedExpression {
                    left_p,
                    expression: Box::new(expr),
                    right_p,
                }
            }
            _ => {
                self.diagnostics.report_unexpected_token(
                    self.tokens_in_the_same_line(self.current()),
                    self.current().clone(),
                    &vec![
                        IntegerToken,
                        FloatPointToken,
                        TrueKeyword,
                        FalseKeyword,
                        IdentifierToken,
                        LeftParenthesisToken,
                    ],
                );
                LiteralExpression {
                    literal_token: self.move_next(),
                }
            }
        }
    }

    fn tokens_in_the_same_line(&self, token: Token) -> Vec<&Token> {
        self.tokens
            .iter()
            .filter(|t| t.line_num == token.line_num)
            .collect::<Vec<&Token>>()
    }

    fn match_token<F>(&self, predict: F, expected: Vec<TokenType>) -> Token
    where
        F: Fn(Token) -> bool,
    {
        if predict(self.current()) {
            self.move_next()
        } else {
            let current = self.move_next();

            self.diagnostics.report_unexpected_token(
                self.tokens_in_the_same_line(current.clone()),
                current.clone(),
                &expected,
            );
            Token::new(
                expected[0],
                current.text,
                current.line_num,
                current.column_num,
            )
        }
    }

    fn peek(&self, offset: usize) -> Token {
        let pos = self.pos();

        if pos + offset >= self.tokens.len() {
            Token::new(TokenType::EndOfFileToken, "".to_string(), 0, 0)
        } else {
            self.tokens[pos + offset].clone()
        }
    }

    fn current(&self) -> Token {
        self.peek(0)
    }

    fn pos(&self) -> usize {
        self.pos.borrow().clone()
    }

    fn move_next(&self) -> Token {
        let c = self.current();
        *self.pos.borrow_mut() += 1;
        c
    }
}
