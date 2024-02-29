use std::rc::Rc;

use BinaryOperatorType::{Addition, Division, Equals, GreaterThan, GreaterThanOrEqual, LessThan, LessThanOrEqual, LogicalAnd, LogicalOr, Multiplication, NotEquals, Remainder, Subtraction};
use ByteCode::{Declaration};
use RuntimeType::{Bool, I32};

use crate::analyze::diagnostic::DiagnosticBag;
use crate::analyze::lex::Token;
use crate::compile::{BinaryOperator, BinaryOperatorType, ByteCode, ConstExpr, FunctionObj, RawType, UnaryOperator, UnaryOperatorType};
use crate::compile::ByteCode::{Assignment, Binary, Block, Break, Call, Continue, FunctionDeclaration, Identifier, If, Literal, Loop, Return, Statement, Unary, While};
use crate::runtime::{Function, RuntimeType, Value};
use crate::runtime::control_command::ControlCommand;
use crate::runtime::runtime_scope::RuntimeScope;
use crate::runtime::Value::fun;
use crate::runtime::variable::Variable;

pub struct Runtime {
    pub scope: Rc<RuntimeScope>,
    pub(crate) diagnostics: DiagnosticBag,
}

impl Runtime {
    pub fn new() -> Self {
        Self {
            scope: Rc::from(RuntimeScope::new_global()),
            diagnostics: DiagnosticBag::new(),
        }
    }

    pub fn with_scope(scope: Rc<RuntimeScope>) -> Self {
        Self {
            scope: Rc::from(RuntimeScope::with_parent(Some(scope))),
            diagnostics: DiagnosticBag::new(),
        }
    }
    pub fn evaluate(&self, expressions: Vec<ByteCode>) -> Vec<Value> {
        let mut results = Vec::new();
        for expression in expressions {
            let res = self.evaluate_expression(expression.clone());

            results.push(res.unwrap());
        }
        self.diagnostics.append(self.scope.diagnostics.clone());
        results
    }

    pub fn evaluate_expression(&self, expression: ByteCode) -> Result<Value, ControlCommand> {
        match expression {
            Statement { expression, .. } => {
                self.evaluate_expression(*expression)?;
                Ok(Value::None)
            }
            Literal { value } => Ok(self.build_literal_value(value)),
            Identifier { identifier } => Ok(self.evaluate_identifier(identifier)),
            Assignment { identifier, expression } => Ok(self.evaluate_assignment(identifier, *expression)),
            Declaration { identifier, expression } => {
                let value = self.evaluate_expression(*expression)?;
                self.scope.set_local(&identifier.text, Variable::new(value));
                Ok(Value::None)
            }
            Unary { op, operand } => Ok(self.evaluate_unary_expression(op, *operand)),
            Binary { op, left, right } => Ok(self.evaluate_binary_expression(*left, op, *right)),
            Block { expressions } => {
                let child_evaluator = self.new_child();
                let res = child_evaluator.evaluate_block(expressions);
                self.diagnostics.append(child_evaluator.diagnostics);
                res
            }
            If { condition, then, r#else } => {
                let condition = self.evaluate_expression(*condition)?;
                if condition == Value::bool(true) {
                    self.evaluate_expression(*then)
                } else if let Some(false_expr) = r#else {
                    self.evaluate_expression(*false_expr)
                } else {
                    Ok(Value::None)
                }
            }
            While { condition, body } => {
                if let Block { expressions } = *body {
                    self.evaluate_while_expression(*condition, expressions);
                }
                Ok(Value::None)
            }
            Loop { body } => {
                let child_evaluator = self.new_child();
                while let Block { expressions } = *body.clone() {
                    if child_evaluator.evaluate_block(expressions.clone()) == Err(ControlCommand::Break) {
                        break
                    }
                }
                Ok(Value::None)
            }
            Break { .. } => Err(ControlCommand::Break),
            Continue { .. } => Err(ControlCommand::Continue),
            FunctionDeclaration { name, function } => self.evaluate_function_declaration(name, function),
            Return { expression } => {
                let value = self.evaluate_expression(*expression)?;
                Err(ControlCommand::Return(value))
            }
            Call { name, function, arguments } => self.evaluate_function_call(name.clone(), arguments.clone()),
            ByteCode::CallBuiltIn { name, arguments } => self.call_built_in(name, arguments),
            ByteCode::FunType { .. } => Ok(Value::None),
        }
    }

    fn evaluate_function_declaration(&self, name: Token, function: FunctionObj) -> Result<Value, ControlCommand> {
        let parent_scope = self.scope.clone();

        let parameter_names = function.param_names.clone();
        let parameter_types = function._type.param_types.clone();
        let parameters: Vec<(String, RuntimeType)> = parameter_names.iter().zip(parameter_types.iter()).map(|(name, r#type)| (name.clone(), match r#type {
            RawType::None => RuntimeType::None,
            RawType::I32 => { I32 }
            RawType::Bool => { Bool }
            RawType::F32 => RuntimeType::F32,
            RawType::FunctionType { .. } => RuntimeType::None,
        })).collect();

        let body = function.body.clone();

        let fun = Function::new(parameters, *body.clone());
        self.scope.declare_function(&name.text, fun.clone());
        Ok(Value::fun { fun })
    }


    fn evaluate_binary_expression(&self, left: ByteCode, op: BinaryOperator, right: ByteCode) -> Value {
        let left_value = self.evaluate_expression(left);
        let right_value = self.evaluate_expression(right);
        match (op.operator_type, left_value, right_value) {
            (Addition, Ok(Value::i32(i)), Ok(Value::i32(j))) => Value::i32(i + j),
            (Addition, Ok(Value::f32(f)), Ok(Value::f32(g))) => Value::f32(f + g),
            (Subtraction, Ok(Value::i32(i)), Ok(Value::i32(j))) => Value::i32(i - j),
            (Subtraction, Ok(Value::f32(f)), Ok(Value::f32(g))) => Value::f32(f - g),
            (Multiplication, Ok(Value::i32(i)), Ok(Value::i32(j))) => Value::i32(i * j),
            (Multiplication, Ok(Value::f32(f)), Ok(Value::f32(g))) => Value::f32(f * g),
            (Division, Ok(Value::i32(i)), Ok(Value::i32(j))) => Value::i32(i / j),
            (Division, Ok(Value::f32(f)), Ok(Value::f32(g))) => Value::f32(f / g),
            (Remainder, Ok(Value::i32(i)), Ok(Value::i32(j))) => Value::i32(i % j),
            (Equals, Ok(Value::i32(i)), Ok(Value::i32(j))) => Value::bool(i == j),
            (Equals, Ok(Value::f32(f)), Ok(Value::f32(g))) => Value::bool(f == g),
            (NotEquals, Ok(Value::i32(i)), Ok(Value::i32(j))) => Value::bool(i != j),
            (NotEquals, Ok(Value::f32(f)), Ok(Value::f32(g))) => Value::bool(f != g),
            (LessThan, Ok(Value::i32(i)), Ok(Value::i32(j))) => Value::bool(i < j),
            (LessThan, Ok(Value::f32(f)), Ok(Value::f32(g))) => Value::bool(f < g),
            (LessThanOrEqual, Ok(Value::i32(i)), Ok(Value::i32(j))) => Value::bool(i <= j),
            (LessThanOrEqual, Ok(Value::f32(f)), Ok(Value::f32(g))) => Value::bool(f <= g),
            (GreaterThan, Ok(Value::i32(i)), Ok(Value::i32(j))) => Value::bool(i > j),
            (GreaterThan, Ok(Value::f32(f)), Ok(Value::f32(g))) => Value::bool(f > g),
            (GreaterThanOrEqual, Ok(Value::i32(i)), Ok(Value::i32(j))) => Value::bool(i >= j),
            (GreaterThanOrEqual, Ok(Value::f32(f)), Ok(Value::f32(g))) => Value::bool(f >= g),
            (LogicalAnd, Ok(Value::bool(b)), Ok(Value::bool(c))) => Value::bool(b && c),
            (LogicalOr, Ok(Value::bool(b)), Ok(Value::bool(c))) => Value::bool(b || c),
            _ => Value::None
        }
    }

    fn evaluate_unary_expression(&self, op: UnaryOperator, operand: ByteCode) -> Value {
        let value = self.evaluate_expression(operand);
        match (op.operator_type, value) {
            (UnaryOperatorType::Negation, Ok(Value::i32(i))) => Value::i32(-i),
            (UnaryOperatorType::Negation, Ok(Value::f32(f))) => Value::f32(-f),
            (UnaryOperatorType::LogicalNegation, Ok(Value::bool(b))) => Value::bool(!b),
            _ => Value::None
        }
    }

    fn build_literal_value(&self, value: ConstExpr) -> Value {
        match value {
            ConstExpr::I32(i) => Value::i32(i),
            ConstExpr::F32(f) => Value::f32(f),
            ConstExpr::Bool(b) => Value::bool(b),
            ConstExpr::None => Value::None,

            _ => { Value::None }
        }
    }

    fn call_built_in(&self, identifier_token: Token, arguments: Vec<ByteCode>) -> Result<Value, ControlCommand> {
        match identifier_token.text.as_str() {
            "print" => {
                for arg in arguments {
                    let value = self.evaluate_expression(arg)?;
                    print!("{}", value);
                }
                Ok(Value::None)
            }
            "println" => {
                for arg in arguments {
                    let value = self.evaluate_expression(arg)?;
                    println!("{}", value);
                }
                Ok(Value::None)
            }
            _ => {
                self.diagnostics.report(format!("Undefined built-in function '{}'", identifier_token.text));
                Ok(Value::None)
            }
        }
    }

    fn evaluate_function_call(&self, identifier_token: Token, arguments: Vec<ByteCode>) -> Result<Value, ControlCommand> {
        let fun = self.scope.get_global(&identifier_token.text);
        if let Some(Variable { value: fun { fun } }) = fun {

            // create a new child evaluator
            let child_evaluator = self.new_child();

            // initialize the parameters
            for (param, arg) in fun.parameters.iter().zip(arguments.iter()) {
                child_evaluator.scope.set_local(&param.0, Variable::new(self.evaluate_expression(arg.clone()).unwrap()));
            }

            // run function body
            let res = match child_evaluator.evaluate_expression(fun.body.clone()) {
                Ok(res) => Ok(res),
                Err(ControlCommand::Return(value)) => Ok(value),
                Err(ControlCommand::Break) => {
                    self.diagnostics.report(format!("'break' is not allowed in function '{}'", identifier_token.text));
                    Ok(Value::None)
                }
                Err(ControlCommand::Continue) => {
                    self.diagnostics.report(format!("'continue' is not allowed in function '{}'", identifier_token.text));
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

    pub fn evaluate_block(&self, block: Vec<ByteCode>) -> Result<Value, ControlCommand> {
        let mut res = Value::None;
        for expr in block {
            res = self.evaluate_expression(expr)?;
        }
        Ok(res)
    }

    fn evaluate_while_expression(&self, condition: ByteCode, block: Vec<ByteCode>) {
        let child_evaluator = self.new_child();
        while self.evaluate_expression(condition.clone()) == Ok(Value::bool(true)) {
            if child_evaluator.evaluate_block(block.clone()) == Err(ControlCommand::Break) {
                break;
            }
        }
    }

    pub fn evaluate_identifier(&self, identifier_token: Token) -> Value {
        if let Some(variable) = self.scope.get_global(&identifier_token.text) {
            variable.value
        } else {
            self.diagnostics.report_undefined_variable(&identifier_token.text);
            Value::None
        }
    }

    pub fn evaluate_assignment(&self, identifier_token: Token, expression: ByteCode) -> Value {
        let name = &identifier_token.text;
        self.scope.try_set_global(name, Variable::new(self.evaluate_expression(expression.clone()).unwrap()));
        Value::None
    }

    fn new_child(&self) -> Self {
        Self::with_scope(Rc::from(RuntimeScope::with_parent(Some(self.scope.clone()))))
    }
}