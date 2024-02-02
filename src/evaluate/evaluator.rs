use TokenType::{AndKeyword, BangToken, MinusToken, OrKeyword, PlusToken, SlashToken, StarToken};
use crate::analyze::diagnostic::DiagnosticBag;
use crate::analyze::lex::{Token, TokenType};
use crate::analyze::lex::TokenType::{FalseKeyword, FloatPointToken, IntegerToken, TrueKeyword};
use crate::analyze::syntax_tree::Expression;
use crate::evaluate::r#type::Type::{F32, I32};
use crate::evaluate::Type::Bool;
use crate::evaluate::value::Value;

pub struct Evaluator {
    pub(crate) diagnostics: DiagnosticBag,
}

impl Evaluator {
    pub fn new() -> Self {
        Self {
            diagnostics: DiagnosticBag::new(),
        }
    }
    pub fn evaluate(&self, expressions: Vec<Expression>) {
        for expression in expressions {
            let res = self.evaluate_expression(expression);
            println!("{}",res);
        }
    }

    pub fn evaluate_expression(&self, expression: Expression) -> Value {
        match expression {
            Expression::LiteralExpression { literal_token } => {
                self.build_literal_value(literal_token)
            }
            Expression::IdentifierExpression { .. } => {
                self.diagnostics.report("Identifier expression not supported yet".to_string());
                Value::None
            }
            Expression::UnaryExpression { operator_token, operand } => {
                let expr = self.evaluate_expression(*operand);
                match (operator_token.token_type, expr.r#type()) {
                    (PlusToken, I32) | (PlusToken, F32) => { expr }
                    (MinusToken, I32) => { Value::i32(-expr.as_i32()) }
                    (MinusToken, F32) => { Value::f32(expr.as_f32()) }
                    (BangToken, Bool) => { Value::bool(!expr.as_bool()) }
                    _ => {
                        self.diagnostics.report_invalid_unary_op(operator_token.clone(), expr.r#type());
                        Value::None
                    }
                }
            }
            Expression::BinaryExpression { left, operator_token, right } => {
                let left = self.evaluate_expression(*left);
                let right = self.evaluate_expression(*right);
                match (left.r#type(), operator_token.token_type, right.r#type()) {
                    (I32, PlusToken, I32) => { Value::i32(left.as_i32() + right.as_i32()) }
                    (F32, PlusToken, F32) => { Value::f32(left.as_f32() + right.as_f32()) }
                    (I32, MinusToken, I32) => { Value::i32(left.as_i32() - right.as_i32()) }
                    (F32, MinusToken, F32) => { Value::f32(left.as_f32() - right.as_f32()) }
                    (I32, StarToken, I32) => { Value::i32(left.as_i32() * right.as_i32()) }
                    (F32, StarToken, F32) => { Value::f32(left.as_f32() * right.as_f32()) }
                    (I32, SlashToken, I32) => { Value::i32(left.as_i32() / right.as_i32()) }
                    (F32, SlashToken, F32) => { Value::f32(left.as_f32() / right.as_f32()) }
                    (Bool, AndKeyword, Bool) => { Value::bool(left.as_bool() && right.as_bool()) }
                    (Bool, OrKeyword, Bool) => { Value::bool(left.as_bool() || right.as_bool()) }
                    (I32, PlusToken, F32) => { Value::f32(left.as_i32() as f32 + right.as_f32()) }
                    (F32, PlusToken, I32) => { Value::f32(left.as_f32() + right.as_i32() as f32) }
                    (I32, MinusToken, F32) => { Value::f32(left.as_i32() as f32 - right.as_f32()) }
                    (F32, MinusToken, I32) => { Value::f32(left.as_f32() - right.as_i32() as f32) }
                    (I32, StarToken, F32) => { Value::f32(left.as_i32() as f32 * right.as_f32()) }
                    (F32, StarToken, I32) => { Value::f32(left.as_f32() * right.as_i32() as f32) }
                    (I32, SlashToken, F32) => { Value::f32(left.as_i32() as f32 / right.as_f32()) }
                    (F32, SlashToken, I32) => { Value::f32(left.as_f32() / right.as_i32() as f32) }
                    _ => {
                        self.diagnostics.report_invalid_binary_op(left.r#type(), operator_token.clone(), right.r#type());
                        Value::None
                    }
                }
            }
            Expression::BracketedExpression { .. } => {
                self.diagnostics.report("Bracketed expression not supported yet".to_string());
                Value::None
            }
            Expression::ParenthesizedExpression { left_p, expression, right_p } => {
                self.evaluate_expression(*expression)
            }
        }
    }

    pub fn build_literal_value(&self, token: Token) -> Value {
        match token.token_type {
            IntegerToken => {
                let value = token.text.parse::<i32>();
                match value {
                    Ok(i) => Value::i32(i),
                    Err(_) => {
                        self.diagnostics.report_invalid_number(token);
                        Value::None
                    }
                }
            }
            FloatPointToken => {
                let value = token.text.parse::<f32>();
                match value {
                    Ok(f) => Value::f32(f),
                    Err(_) => {
                        self.diagnostics.report_invalid_number(token);
                        Value::None
                    }
                }
            }
            TrueKeyword | FalseKeyword => {
                let value = token.token_type == TrueKeyword;
                Value::bool(value)
            }
            _ => {
                self.diagnostics.report_invalid_literal(token);
                Value::None
            }
        }
    }
}