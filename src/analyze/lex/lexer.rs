use std::cell::RefCell;

use TokenType::{EndOfFileToken, SemicolonToken, ValKeyword, VarKeyword};

use crate::analyze::lex::token::Token;
use crate::analyze::lex::token::TokenType;
use crate::analyze::lex::token::TokenType::AndKeyword;
use crate::analyze::lex::token::TokenType::BadToken;
use crate::analyze::lex::token::TokenType::BangEqualsToken;
use crate::analyze::lex::token::TokenType::BangToken;
use crate::analyze::lex::token::TokenType::EqualsToken;
use crate::analyze::lex::token::TokenType::FalseKeyword;
use crate::analyze::lex::token::TokenType::FloatPointToken;
use crate::analyze::lex::token::TokenType::IdentifierToken;
use crate::analyze::lex::token::TokenType::IntegerToken;
use crate::analyze::lex::token::TokenType::LeftBraceToken;
use crate::analyze::lex::token::TokenType::LeftParenthesisToken;
use crate::analyze::lex::token::TokenType::MinusToken;
use crate::analyze::lex::token::TokenType::OrKeyword;
use crate::analyze::lex::token::TokenType::PlusToken;
use crate::analyze::lex::token::TokenType::RightBraceToken;
use crate::analyze::lex::token::TokenType::RightParenthesisToken;
use crate::analyze::lex::token::TokenType::SlashToken;
use crate::analyze::lex::token::TokenType::StarToken;
use crate::analyze::lex::token::TokenType::TrueKeyword;
use crate::analyze::lex::token::TokenType::WhitespaceToken;
use crate::analyze::lex::TokenType::EqualsEqualsToken;

pub struct Lexer {
    chars: Vec<char>,
    pos: RefCell<usize>,
    line_number: RefCell<usize>,
    column_number: RefCell<usize>,
}

impl Lexer {
    pub fn new(text: &str) -> Self {
        let chars = text.chars().collect();
        Self {
            chars,
            pos: RefCell::from(0),
            line_number: RefCell::from(1),
            column_number: RefCell::from(1),
        }
    }

    fn line_number(&self) -> usize {
        *self.line_number.borrow()
    }
    fn column_number(&self) -> usize {
        *self.column_number.borrow()
    }


    pub(crate) fn lex(&self) -> Vec<Token> {
        let mut tokens = Vec::new();
        while let Some(c) = self.current() {
            let token = if c.is_ascii_digit() {
                self.lex_number()
            } else if c.is_alphabetic() {
                self.lex_keyword_or_identifier()
            } else if c.is_whitespace() {
                let white_space = self.lex_white_spase();
                if white_space.text.contains('\n') {
                    *self.line_number.borrow_mut() += 1;
                    *self.column_number.borrow_mut() = 1;
                }
                white_space
            } else {
                self.lex_operator()
            };
            tokens.push(token);
        }
        tokens.push(Token::new(EndOfFileToken, "".to_string(), self.line_number(), self.column_number()));
        tokens.retain(|t| t.token_type != WhitespaceToken);
        tokens
    }

    fn lex_number(&self) -> Token {
        // let start = self.pos.borrow();
        let mut text = self.lex_while(|c| c.is_ascii_digit() || c == '.' || c == '_');
        text = text.replace('_', "");
        let token_type = if text.contains('.') {
            FloatPointToken
        } else {
            IntegerToken
        };
        Token::new(token_type, text, self.line_number(), self.column_number())
    }
    fn lex_keyword_or_identifier(&self) -> Token {
        let text = self.lex_while(|c| c.is_alphanumeric());
        let token_type = match text.as_str() {
            "and" => AndKeyword,
            "or" => OrKeyword,
            "true" => TrueKeyword,
            "false" => FalseKeyword,
            "val" => ValKeyword,
            "var" => VarKeyword,
            "if" => TokenType::IfKeyword,
            "else" => TokenType::ElseKeyword,
            _ => IdentifierToken,
        };
        Token::new(token_type, text, self.line_number(), self.column_number())
    }
    fn lex_operator(&self) -> Token {
        match self.current().unwrap() {
            '+' => {
                self.move_next();
                Token::new(PlusToken, "+".to_string(), self.line_number(), self.column_number())
            }
            '-' => {
                self.move_next();
                Token::new(MinusToken, "-".to_string(), self.line_number(), self.column_number())
            }
            '*' => {
                self.move_next();
                Token::new(StarToken, "*".to_string(), self.line_number(), self.column_number())
            }
            '/' => {
                self.move_next();
                Token::new(SlashToken, "/".to_string(), self.line_number(), self.column_number())
            }
            '(' => {
                self.move_next();
                Token::new(LeftParenthesisToken, "(".to_string(), self.line_number(), self.column_number())
            }
            ')' => {
                self.move_next();
                Token::new(RightParenthesisToken, ")".to_string(), self.line_number(), self.column_number())
            }
            '{' => {
                self.move_next();
                Token::new(LeftBraceToken, "{".to_string(), self.line_number(), self.column_number())
            }
            '}' => {
                self.move_next();
                Token::new(RightBraceToken, "}".to_string(), self.line_number(), self.column_number())
            }
            ';' => {
                self.move_next();
                Token::new(SemicolonToken, ";".to_string(), self.line_number(), self.column_number())
            }
            '=' if self.next().is_some() && *self.next().unwrap() == '=' => {
                self.move_next();
                self.move_next();
                Token::new(EqualsEqualsToken, "==".to_string(), self.line_number(), self.column_number())
            }
            '=' => {
                self.move_next();
                Token::new(EqualsToken, "=".to_string(), self.line_number(), self.column_number())
            }
            '!' if self.next().is_some() && *self.next().unwrap() == '=' => {
                self.move_next();
                self.move_next();
                Token::new(BangEqualsToken, "!=".to_string(), self.line_number(), self.column_number())
            }
            '!' => {
                self.move_next();
                Token::new(BangToken, "!".to_string(), self.line_number(), self.column_number())
            }
            c => {
                self.move_next();
                Token::new(BadToken, c.to_string(), self.line_number(), self.column_number())
            }
        }
    }

    fn lex_white_spase(&self) -> Token {
        let text = self.lex_while(|c| c.is_whitespace());
        Token::new(WhitespaceToken, text, self.line_number(), self.column_number())
    }
    fn current(&self) -> Option<&char> {
        self.peek(0)
    }
    fn next(&self) -> Option<&char> {
        self.peek(1)
    }
    fn peek(&self, offset: usize) -> Option<&char> {
        let index = offset + *self.pos.borrow();
        self.chars.get(index)
    }

    fn move_next(&self) -> &char {
        let c = self.current().unwrap();
        *self.pos.borrow_mut() += 1;
        *self.column_number.borrow_mut() += 1;
        c
    }
    fn lex_while<F>(&self, predict: F) -> String
    where
        F: Fn(char) -> bool,
    {
        let mut text = String::new();
        loop {
            match self.current() {
                Some(c) if predict(*c) => {
                    text.push(*c);
                    self.move_next();
                }
                _ => {
                    break;
                }
            }
        }
        text
    }
}
