use std::rc::Rc;

use ControlCommand::{Break, Continue};
use Expression::{AssignmentExpression, BinaryExpression, BracketedExpression, BreakExpression, DeclarationExpression, FunctionCallExpression, FunctionDeclarationExpression, IdentifierExpression, IfExpression, LiteralExpression, LoopExpression, ParenthesizedExpression, ReturnExpression, Statement, UnaryExpression, WhileExpression};
use TokenType::{AndKeyword, BangToken, EqualsEqualsToken, GreatThanToken, LessThanToken, MinusToken, OrKeyword, PlusToken, SlashToken, StarToken, VarKeyword};
use Value::bool;

use crate::analyze::diagnostic::DiagnosticBag;
use crate::analyze::lex::{Token, TokenType};
use crate::analyze::lex::TokenType::{BangEqualsToken, FalseKeyword, FloatPointToken, IntegerToken, TrueKeyword};
use crate::analyze::syntax_tree::{Block, Expression};
use crate::analyze::syntax_tree::Expression::ContinueExpression;
use crate::evaluate::control_command::ControlCommand;
use crate::evaluate::control_command::ControlCommand::Return;
use crate::evaluate::function::Function;
use crate::evaluate::r#type::Type::{F32, I32};
use crate::evaluate::runtime_scope::RuntimeScope;
use crate::evaluate::Type::Bool;
use crate::evaluate::value::Value;
use crate::evaluate::variable::Variable;

pub struct Evaluator {
    pub scope: Rc<RuntimeScope>,
    pub(crate) diagnostics: DiagnosticBag,
}

impl Evaluator {
    pub fn new() -> Self {
        Self {
            scope: Rc::from(RuntimeScope::new_global()),
            diagnostics: DiagnosticBag::new(),
        }
    }

    pub fn with_scope(scope: RuntimeScope) -> Self {
        Self {
            scope: Rc::from(RuntimeScope::with_parent(Some(Rc::from(scope)))),
            diagnostics: DiagnosticBag::new(),
        }
    }
    pub fn evaluate(&self, expressions: Vec<Expression>) -> Vec<Value> {
        let mut results = Vec::new();
        for expression in expressions {
            let res = self.evaluate_expression(expression.clone());

            if !self.diagnostics.is_empty() {
                self.diagnostics.print();
                println!("at: \n{}", expression);
                println!();
                self.diagnostics.clear();
            }


            results.push(res.unwrap());
        }
        self.diagnostics.append(self.scope.diagnostics.clone());
        results
    }

    pub fn evaluate_expression(&self, expression: Expression) -> Result<Value, ControlCommand> {
        match expression {
            Statement { expression, .. } => {
                self.evaluate_expression(*expression)?;
                Ok(Value::None)
            }
            LiteralExpression { literal_token } => {
                Ok(self.build_literal_value(literal_token))
            }
            IdentifierExpression { identifier_token } => {
                Ok(self.evaluate_identifier(identifier_token))
            }
            DeclarationExpression { .. } => {
                Ok(self.evaluate_declaration(expression))
            }
            AssignmentExpression {
                identifier_token,
                expression, ..
            } => {
                Ok(self.evaluate_assignment(identifier_token, *expression))
            }
            UnaryExpression { operator_token, operand } => {
                Ok(self.evaluate_unary_expression(operator_token, *operand))
            }
            BinaryExpression { left, operator_token, right } => {
                Ok(self.evaluate_binary_expression(*left, operator_token, *right))
            }
            BracketedExpression { block, .. } => {
                let child_evaluator = self.new_child();
                let res = child_evaluator.evaluate_block(block);
                self.diagnostics.append(child_evaluator.diagnostics);
                res
            }
            ParenthesizedExpression { expression, .. } => {
                self.evaluate_expression(*expression)
            }
            IfExpression { if_token, condition, true_expr, else_token, false_expr } => {
                let condition = self.evaluate_expression(*condition)?;
                if condition == bool(true) {
                    self.evaluate_expression(*true_expr)
                } else if let Some(false_expr) = false_expr {
                    self.evaluate_expression(*false_expr)
                } else {
                    Ok(Value::None)
                }
            }
            WhileExpression { condition, body, .. } => {
                if let BracketedExpression { block, .. } = *body {
                    self.evaluate_while_expression(*condition, block);
                }
                Ok(Value::None)
            }
            LoopExpression { body, .. } => {
                while let BracketedExpression { ref block, .. } = *body {
                    let child_evaluator = self.new_child();
                    if child_evaluator.evaluate_block(block.clone()) == Err(Break) {
                        break
                    }
                }
                Ok(Value::None)
            }
            BreakExpression { .. } => Err(Break),
            ContinueExpression { .. } => Err(Continue),
            FunctionDeclarationExpression {
                ref identifier_token,
                ref parameters,
                ref body, ..
            } => {
                let fun = Function::new(parameters.iter().map(|p| p.text.clone()).collect(), *body.clone(), expression.clone());
                self.scope.declare_function(&identifier_token.text, Rc::new(fun));
                Ok(Value::None)
            }
            ReturnExpression { expression, .. } => {
                let value = self.evaluate_expression(*expression)?;
                Err(ControlCommand::Return(value))
            }
            FunctionCallExpression { ref identifier_token, ref arguments, .. } => {
                let fun = self.scope.get_global_function(&identifier_token.text);
                if let Some(fun) = fun {
                    let child_evaluator = self.new_child();
                    for i in 0..fun.parameters.len() {
                        let arg_expr = arguments.get(i).unwrap();
                        let variable_name = fun.parameters.get(i).unwrap();
                        let arg_value = child_evaluator.evaluate_expression(arg_expr.clone())?;
                        child_evaluator.scope.set_local_variable(variable_name, Variable::new_mutable(arg_value, expression.clone()))
                    }
                    // println!("{}", child_evaluator.scope.variables_to_string());
                    let res = match child_evaluator.evaluate_expression(fun.body.clone()) {
                        Ok(res) => Ok(res),
                        Err(Return(value)) => Ok(value),
                        Err(Break) => {
                            self.diagnostics.report_break_function_call(&identifier_token.text,fun);
                            Ok(Value::None)
                        }
                        Err(Continue)=> {
                            self.diagnostics.report_continue_function_call(&identifier_token.text,fun);
                            Ok(Value::None)
                        }
                    };
                    self.diagnostics.append(child_evaluator.diagnostics);
                    res
                } else {
                    self.diagnostics.report_undefined_function(&identifier_token.text);
                    Ok(Value::None)
                }
            }
        }
    }

    pub fn evaluate_block(&self, block: Block) -> Result<Value, ControlCommand> {
        let mut res = Value::None;
        for expr in block.expressions.take().into_iter() {
            res = self.evaluate_expression(expr)?;
        }
        Ok(res)
    }

    fn evaluate_while_expression(&self, condition: Expression, block: Block) {
        let child_evaluator = self.new_child();
        while self.evaluate_expression(condition.clone()) == Ok(bool(true)) {
            if child_evaluator.evaluate_block(block.clone()) == Err(Break) {
                break;
            }
        }
    }

    pub fn evaluate_identifier(&self, identifier_token: Token) -> Value {
        if let Some(variable) = self.scope.get_global_variable(&identifier_token.text) {
            variable.value
        } else {
            self.diagnostics.report_undefined_variable(&identifier_token.text);
            Value::None
        }
    }

    pub fn evaluate_declaration(&self, declaration_expression: Expression) -> Value {
        if let DeclarationExpression { declaration_token, identifier_token, expression, .. } = declaration_expression.clone() {
            let value = self.evaluate_expression(*expression.clone()).unwrap();
            let mutable = declaration_token.token_type == VarKeyword;
            let variable = Variable::new(mutable, value, declaration_expression.clone());
            self.scope.set_local_variable(&identifier_token.text, variable);
        }

        Value::None
    }

    pub fn evaluate_assignment(&self, identifier_token: Token, expression: Expression) -> Value {
        let name = &identifier_token.text;
        if let Some(v) = self.scope.get_global_variable(name) {
            if v.mutable {
                let value = self.evaluate_expression(expression).unwrap();
                self.scope.set_global_variable(name, Variable::new_mutable(value, v.declared_expression));
            } else {
                self.diagnostics.report_immutable_variable(identifier_token.clone(), v.declared_expression.clone());
            }
        } else { self.diagnostics.report_undefined_variable(&identifier_token.text); }
        Value::None
    }

    pub fn evaluate_binary_expression(&self, left: Expression, operator_token: Token, right: Expression) -> Value {
        let left = self.evaluate_expression(left).unwrap();
        let right = self.evaluate_expression(right).unwrap();
        match (left.r#type(), operator_token.token_type, right.r#type()) {
            (I32, PlusToken, I32) => { Value::i32(left.as_i32() + right.as_i32()) }
            (F32, PlusToken, F32) => { Value::f32(left.as_f32() + right.as_f32()) }
            (I32, MinusToken, I32) => { Value::i32(left.as_i32() - right.as_i32()) }
            (F32, MinusToken, F32) => { Value::f32(left.as_f32() - right.as_f32()) }
            (I32, StarToken, I32) => { Value::i32(left.as_i32() * right.as_i32()) }
            (F32, StarToken, F32) => { Value::f32(left.as_f32() * right.as_f32()) }
            (I32, SlashToken, I32) => { Value::i32(left.as_i32() / right.as_i32()) }
            (F32, SlashToken, F32) => { Value::f32(left.as_f32() / right.as_f32()) }
            (Bool, AndKeyword, Bool) => { bool(left.as_bool() && right.as_bool()) }
            (Bool, OrKeyword, Bool) => { bool(left.as_bool() || right.as_bool()) }
            (I32, PlusToken, F32) => { Value::f32(left.as_i32() as f32 + right.as_f32()) }
            (F32, PlusToken, I32) => { Value::f32(left.as_f32() + right.as_i32() as f32) }
            (I32, MinusToken, F32) => { Value::f32(left.as_i32() as f32 - right.as_f32()) }
            (F32, MinusToken, I32) => { Value::f32(left.as_f32() - right.as_i32() as f32) }
            (I32, StarToken, F32) => { Value::f32(left.as_i32() as f32 * right.as_f32()) }
            (F32, StarToken, I32) => { Value::f32(left.as_f32() * right.as_i32() as f32) }
            (I32, SlashToken, F32) => { Value::f32(left.as_i32() as f32 / right.as_f32()) }
            (F32, SlashToken, I32) => { Value::f32(left.as_f32() / right.as_i32() as f32) }
            (I32, LessThanToken, I32) => { bool(left.as_i32() < right.as_i32()) }
            (F32, LessThanToken, F32) => { bool(left.as_f32() < right.as_f32()) }
            (I32, LessThanToken, F32) => { bool((left.as_i32() as f32) < right.as_f32()) }
            (F32, LessThanToken, I32) => { bool(left.as_f32() < right.as_i32() as f32) }
            (I32, GreatThanToken, I32) => { bool(left.as_i32() > right.as_i32()) }
            (F32, GreatThanToken, F32) => { bool(left.as_f32() > right.as_f32()) }
            (I32, GreatThanToken, F32) => { bool((left.as_i32() as f32) > right.as_f32()) }
            (F32, GreatThanToken, I32) => { bool(left.as_f32() > right.as_i32() as f32) }

            (I32, TokenType::PercentToken, I32) => { Value::i32(left.as_i32() % right.as_i32()) }

            // (Bool, EqualsEqualsToken, Bool) => { Value::bool(left.as_bool() == right.as_bool()) }
            // (I32, EqualsEqualsToken, I32) => { Value::bool(left.as_i32() == right.as_i32()) }
            // (F32, EqualsEqualsToken, F32) => { Value::bool(left.as_f32() == right.as_f32()) }
            // (Bool, BangEqualsToken, Bool) => { Value::bool(left.as_bool() != right.as_bool()) }
            // (I32, BangEqualsToken, I32) => { Value::bool(left.as_i32() != right.as_i32()) }
            // (F32, BangEqualsToken, F32) => { Value::bool(left.as_f32() != right.as_f32()) }
            (_, EqualsEqualsToken, _) => { bool(left == right) }
            (_, BangEqualsToken, _) => { bool(left != right) }
            _ => {
                self.diagnostics.report_invalid_binary_op(left.r#type(), operator_token.clone(), right.r#type());
                Value::None
            }
        }
    }

    pub fn evaluate_unary_expression(&self, operator_token: Token, operand: Expression) -> Value {
        let expr = self.evaluate_expression(operand).unwrap();
        match (operator_token.token_type, expr.r#type()) {
            (PlusToken, I32) | (PlusToken, F32) => { expr }
            (MinusToken, I32) => { Value::i32(-expr.as_i32()) }
            (MinusToken, F32) => { Value::f32(expr.as_f32()) }
            (BangToken, Bool) => { bool(!expr.as_bool()) }
            _ => {
                self.diagnostics.report_invalid_unary_op(operator_token.clone(), expr.r#type());
                Value::None
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
                bool(value)
            }
            _ => {
                self.diagnostics.report_invalid_literal(token);
                Value::None
            }
        }
    }

    fn new_child(&self) -> Self {
        Self::with_scope(RuntimeScope::with_parent(Some(self.scope.clone())))
    }
}