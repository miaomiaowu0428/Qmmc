use crate::analyze::lex::line::Line;
use crate::analyze::lex::source_file::SourceFile;
use crate::analyze::lex::token::TokenType::AndKeyword;
use crate::analyze::lex::token::TokenType::BadToken;
use crate::analyze::lex::token::TokenType::BangEqualsToken;
use crate::analyze::lex::token::TokenType::BangToken;
use crate::analyze::lex::token::TokenType::EqualsToken;
use crate::analyze::lex::token::TokenType::FalseKeyword;
use crate::analyze::lex::token::TokenType::FloatPointToken;
use crate::analyze::lex::token::TokenType::IdentifierToken;
use crate::analyze::lex::token::TokenType::IntegerToken;
use crate::analyze::lex::token::TokenType::MinusToken;
use crate::analyze::lex::token::TokenType::OrKeyword;
use crate::analyze::lex::token::TokenType::PlusToken;
use crate::analyze::lex::token::TokenType::SlashToken;
use crate::analyze::lex::token::TokenType::StarToken;
use crate::analyze::lex::token::TokenType::WhitespaceToken;
use crate::analyze::lex::token::TokenType::LeftParenthesisToken;
use crate::analyze::lex::token::TokenType::LeftBraceToken;
use crate::analyze::lex::token::TokenType::EndLineToken;
use crate::analyze::lex::token::TokenType::RightBraceToken;
use crate::analyze::lex::token::TokenType::RightParenthesisToken;
use crate::analyze::lex::token::TokenType::TrueKeyword;
use crate::analyze::lex::token::Token;
use crate::analyze::lex::token::TokenType;
use std::cell::RefCell;
use TokenType::{SemicolonToken, ValKeyword, VarKeyword};

pub struct Lexer {
    chars: Vec<char>,
    pos: RefCell<usize>,
}

impl Lexer {
    pub fn new(text: &str) -> Self {
        let chars = text.chars().collect();
        Self {
            chars,
            pos: RefCell::from(0),
        }
    }
    pub(crate) fn lex(&self) -> SourceFile {
        let mut lines: Vec<Line> = Vec::new();
        loop {
            if self.chars.len() <= *self.pos.borrow() {
                break;
            }

            let line = self.lex_line();
            if line.tokens.borrow().len() > 0 {
                lines.push(line);
            }
        }
        lines.retain(|l| {
            l.tokens.borrow().len() > 0
                && match l.tokens.borrow()[0].token_type {
                WhitespaceToken => false,
                BadToken => false,
                EndLineToken => false,
                // SemicolonToken => false,
                _ => true,
            }
        });
        lines.into()
    }
    fn lex_line(&self) -> Line {
        let mut tokens: Vec<Token> = Vec::new();
        loop {
            match self.current() {
                Some(c) if c.is_digit(10) => {
                    let number_token = self.lex_number();
                    tokens.push(number_token);
                }
                Some(c) if c.is_alphabetic() => tokens.push(self.lex_keyword_or_identifier()),
                Some(c) if *c == '{' => {
                    tokens.push(self.lex_operator());
                    break;
                }
                Some(c) if *c == ';' => {
                    tokens.push(self.lex_operator());
                    break;
                }
                Some(c) if *c == '}' && tokens.len() == 0 => {
                    tokens.push(self.lex_operator());
                    break;
                }
                Some(c)if *c == '}'
                    && self.peek(1).is_some()
                    && *self.peek(1).unwrap() == ';'
                    && tokens.len() == 0
                => {
                    tokens.push(self.lex_operator());
                    tokens.push(self.lex_operator());
                    break;
                }
                Some(c) if *c == '}' => {
                    break;
                }

                Some(c) if c.is_whitespace() => {
                    self.lex_white_spase();
                }
                Some(_) => tokens.push(self.lex_operator()),
                None => {
                    break;
                }
            }
        }
        // tokens.push(Token::new(EndLineToken, "end-line".to_string()));
        Line::new(tokens)
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
        Token::new(token_type, text)
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
            _ => IdentifierToken,
        };
        Token::new(token_type, text)
    }
    fn lex_operator(&self) -> Token {
        match self.current().unwrap() {
            '+' => {
                self.move_next();
                Token::new(PlusToken, "+".to_string())
            }
            '-' => {
                self.move_next();
                Token::new(MinusToken, "-".to_string())
            }
            '*' => {
                self.move_next();
                Token::new(StarToken, "*".to_string())
            }
            '/' => {
                self.move_next();
                Token::new(SlashToken, "/".to_string())
            }
            '(' => {
                self.move_next();
                Token::new(LeftParenthesisToken, "(".to_string())
            }
            ')' => {
                self.move_next();
                Token::new(RightParenthesisToken, ")".to_string())
            }
            '{' => {
                self.move_next();
                Token::new(LeftBraceToken, "{".to_string())
            }
            '}' => {
                self.move_next();
                Token::new(RightBraceToken, "}".to_string())
            }
            ';' => {
                self.move_next();
                Token::new(SemicolonToken, ";".to_string())
            }
            '=' if self.next().is_some() && *self.next().unwrap() == '=' => {
                self.move_next();
                self.move_next();
                Token::new(BangEqualsToken, "==".to_string())
            }
            '=' => {
                self.move_next();
                Token::new(EqualsToken, "=".to_string())
            }
            '!' if self.next().is_some() && *self.next().unwrap() == '=' => {
                self.move_next();
                self.move_next();
                Token::new(BangEqualsToken, "!=".to_string())
            }
            '!' => {
                self.move_next();
                Token::new(BangToken, "!".to_string())
            }
            c => {
                self.move_next();
                Token::new(BadToken, c.to_string())
            }
        }
    }

    fn lex_white_spase(&self) -> Token {
        let text = self.lex_while(|c| c.is_whitespace());
        Token::new(WhitespaceToken, text)
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
