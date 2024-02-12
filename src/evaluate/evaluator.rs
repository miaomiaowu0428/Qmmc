use std::cell::RefCell;
use std::collections::HashMap;
use Expression::{AssignmentExpression, BinaryExpression, BracketedExpression, DeclarationExpression, IdentifierExpression, LiteralExpression, ParenthesizedExpression, Statement, UnaryExpression};
use TokenType::{AndKeyword, BangEqualsToken, BangToken, EqualsEqualsToken, MinusToken, OrKeyword, PlusToken, SlashToken, StarToken, ValKeyword, VarKeyword};
use crate::analyze::diagnostic::DiagnosticBag;
use crate::analyze::lex::{Token, TokenType};
use crate::analyze::lex::TokenType::{FalseKeyword, FloatPointToken, IntegerToken, TrueKeyword};
use crate::analyze::syntax_tree::Expression;
use crate::evaluate::r#type::Type::{F32, I32};
use crate::evaluate::runtime_scope::ValueMap;
use crate::evaluate::Type::Bool;
use crate::evaluate::value::Value;
use crate::evaluate::variable::Variable;

pub struct Evaluator {
    pub values: RefCell<ValueMap>,
    pub(crate) diagnostics: DiagnosticBag,
}

impl Evaluator {
    pub fn new() -> Self {
        Self {
            values: RefCell::new(HashMap::new()),
            diagnostics: DiagnosticBag::new(),
        }
    }
    pub fn evaluate(&self, expressions: Vec<Expression>)->Vec<Value> {
        let mut results = Vec::new();
        for expression in expressions {
            let res = self.evaluate_expression(expression);
            results.push(res);
        }
        results
    }

    pub fn evaluate_expression(&self, expression: Expression) -> Value {
        match expression {
            Statement { expression, .. } => {
                self.evaluate_expression(*expression);
                Value::None
            }
            LiteralExpression { literal_token } => {
                self.build_literal_value(literal_token)
            }
            IdentifierExpression { identifier_token } => {
                let name = identifier_token.text.clone();
                let values = self.values.borrow();
                if values.contains_key(&name.clone()) {
                    values.get(&name).unwrap().value.clone()
                } else {
                    self.diagnostics.report_undefined_variable(identifier_token);
                    Value::None
                }
            }
            DeclarationExpression {
                declaration_token,
                identifier_token,
                expression,
                ..
            } => {
                let name = identifier_token.text.clone();
                let mutable = match declaration_token.token_type {
                    VarKeyword => true,
                    ValKeyword => false,
                    _ => panic!("can only declare a variable with var or val. unexpected {}", declaration_token.token_type),
                };
                let variable = Variable::new(mutable, self.evaluate_expression(*expression));

                self.values.borrow_mut().insert(name, variable);

                Value::None
            }
            AssignmentExpression {
                identifier_token,
                expression, ..
            } => {
                let mut values = self.values.borrow_mut();
                let name = identifier_token.text.clone();
                match values.get_mut(&name) {
                    Some(value) => {
                        if value.mutable {
                            let expr = self.evaluate_expression(*expression);
                            *value = Variable::new(true, expr);
                        } else {
                            self.diagnostics.report_immutable_variable(identifier_token);
                        }
                    }
                    None => {
                        self.diagnostics.report_undefined_variable(identifier_token);
                    }
                }
                Value::None
            }
            UnaryExpression { operator_token, operand } => {
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
            BinaryExpression { left, operator_token, right } => {
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
                    (Bool, EqualsEqualsToken, Bool) => { Value::bool(left.as_bool() == right.as_bool()) }
                    (I32, EqualsEqualsToken, I32) => { Value::bool(left.as_i32() == right.as_i32()) }
                    (F32, EqualsEqualsToken, F32) => { Value::bool(left.as_f32() == right.as_f32()) }
                    (Bool, BangEqualsToken, Bool) => { Value::bool(left.as_bool() != right.as_bool()) }
                    (I32, BangEqualsToken, I32) => { Value::bool(left.as_i32() != right.as_i32()) }
                    (F32, BangEqualsToken, F32) => { Value::bool(left.as_f32() != right.as_f32()) }
                    _ => {
                        self.diagnostics.report_invalid_binary_op(left.r#type(), operator_token.clone(), right.r#type());
                        Value::None
                    }
                }
            }
            BracketedExpression { block, .. } => {
                let mut res = Value::None;
                for expr in block.expressions.take().into_iter() {
                    res = self.evaluate_expression(expr);
                }
                res
            }
            ParenthesizedExpression { left_p, expression, right_p } => {
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