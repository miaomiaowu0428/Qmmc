use std::rc::Rc;

use colored::Colorize;
use lazy_static::lazy_static;

use ByteCode::{Break, FunType};
use TokenType::VarKeyword;

use crate::analyze::diagnostic::DiagnosticBag;
use crate::analyze::lex::{Token, TokenType};
use crate::analyze::syntax_tree::{Block, Expression, IdentifierTypePair};
use crate::compile::binary_operator::BinaryOperator;
use crate::compile::ByteCode::Declaration;
use crate::compile::checked_expression::{ByteCode, ConstExpr, FunctionObj};
use crate::compile::compile_time_scope::CompileTimeScope;
use crate::compile::r#type::FunctionType;
use crate::compile::RawType;
use crate::compile::RawType::{Bool, F32, I32};
use crate::compile::unary_operator::UnaryOperator;
use crate::compile::variable_symbol::VariableSymbol;

lazy_static!(static ref BUILT_IN_FUNCTION_NAME:Vec<String> = vec![
    "print".to_string(),
    "println".to_string()
];);





pub struct Compiler {
    pub scope: Rc<CompileTimeScope>,
    pub diagnostics: DiagnosticBag,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            scope: Rc::new(CompileTimeScope::new_global()),
            diagnostics: DiagnosticBag::new(),
        }
    }


    pub fn new_child(&self) -> Self {
        Self {
            scope: Rc::new(CompileTimeScope::with_parent(Some(self.scope.clone()))),
            diagnostics: DiagnosticBag::new(),
        }
    }

    pub fn analyse(&self, expressions: Vec<Expression>) -> Vec<ByteCode> {
        expressions.iter().map(|e| self.check_expression(e.clone())).collect()
    }
    pub fn check_expression(&self, expression: Expression) -> ByteCode {
        match expression {
            Expression::Statement { .. } => ByteCode::Statement { expression: Box::new(self.check_expression(expression.clone())) },
            Expression::LiteralExpression { literal_token } => ByteCode::Literal { value: ConstExpr::from(literal_token) },
            Expression::IdentifierExpression { identifier_token } => self.check_identifier_expression(identifier_token),
            Expression::UnaryExpression { operator_token, operand } => self.check_unary_expression(operator_token, operand),
            Expression::BinaryExpression { left, operator_token, right } => self.check_binary_expression(left, operator_token, right),
            Expression::BracketedExpression { block, .. } => self.check_block(block),
            Expression::ParenthesizedExpression { expression, .. } => self.check_expression(*expression),
            Expression::VarDeclarationExpression { declaration_token, identifier_token, equals_token, assigment_expr } => self.check_var_declaration_expression(declaration_token, identifier_token, equals_token, assigment_expr),
            Expression::AssignmentExpression { identifier_token, expression, .. } => self.check_assignment_expression(&identifier_token, expression),
            Expression::IfExpression { if_token, condition, true_expr, else_token, false_expr } => self.check_if_expression(if_token, condition, true_expr, else_token, false_expr),
            Expression::WhileExpression { while_token, condition, body, .. } => self.check_while_expression(while_token, condition, body),
            Expression::LoopExpression { body, .. } => self.check_loop_expression(body),
            Expression::BreakExpression { .. } => Break,
            Expression::ContinueExpression { .. } => ByteCode::Continue,
            Expression::FunctionDeclarationExpression { identifier_token, parameters, res_type_description, body, .. } => self.check_function_declaration(identifier_token, parameters, res_type_description, body),
            Expression::ReturnExpression { expression, .. } => self.check_return_expression(expression),
            Expression::FunctionCallExpression { identifier_token, arguments, .. } => self.check_func_call(identifier_token, arguments),
            Expression::FunctionTypeExpression { parameter_types, return_type, .. } => self.check_function_type_expression(parameter_types, return_type),
        }
    }

    fn check_return_expression(&self, expression: Box<Expression>) -> ByteCode {
        let checked_expression = self.check_expression(*expression);
        ByteCode::Return { expression: Box::new(checked_expression) }
    }

    fn check_loop_expression(&self, body: Box<Expression>) -> ByteCode {
        let checked_body = self.check_expression(*body);
        ByteCode::Loop { body: Box::new(checked_body) }
    }

    fn check_while_expression(&self, while_token: Token, condition: Box<Expression>, body: Box<Expression>) -> ByteCode {
        let checked_condition = self.check_expression(*condition);
        let checked_body = self.check_expression(*body);
        if self.type_of(&checked_condition.clone()) != Bool {
            self.diagnostics.report(format!("{} at ({},{}): condition is not a boolean in while expression", "Error".red(), while_token.line_num, while_token.column_num));
        }
        ByteCode::While { condition: Box::new(checked_condition), body: Box::new(checked_body) }
    }

    fn check_if_expression(&self, if_token: Token, condition: Box<Expression>, true_expr: Box<Expression>, else_token: Option<Token>, false_expr: Option<Box<Expression>>) -> ByteCode {
        let checked_condition = self.check_expression(*condition);
        let checked_true = self.check_expression(*true_expr);
        let checked_false = match else_token {
            Some(else_token) => Some(self.check_expression(*false_expr.unwrap())),
            None => None,
        };
        if self.type_of(&checked_condition.clone()) != Bool {
            self.diagnostics.report(format!("{} at ({},{}): condition is not a boolean in if expression", "Error".red(), if_token.line_num, if_token.column_num));
        }
        ByteCode::If { condition: Box::new(checked_condition), then: Box::new(checked_true), r#else: checked_false.map(Box::new) }
    }

    fn check_assignment_expression(&self, identifier_token: &Token, expression: Box<Expression>) -> ByteCode {
        let checked_expression = self.check_expression(*expression);
        let r#type = self.type_of(&checked_expression);
        let symbol = self.scope.get_global(&identifier_token.text);
        match symbol {
            Some(s) if s.r#type == r#type && s.mutable => {
                ByteCode::Assignment { identifier: identifier_token.clone(), expression: Box::new(checked_expression) }
            }
            Some(s)
            if s.r#type == r#type
                && s.mutable == false
                && s.initialized == false => {
                self.scope.try_set_global(&identifier_token.text, VariableSymbol::init_as_immut(r#type.clone(), ));
                ByteCode::Assignment { identifier: identifier_token.clone(), expression: Box::new(checked_expression) }
            }
            Some(s) if s.r#type != r#type && s.mutable => {
                self.diagnostics.report(format!(
                    "type mismatch in assignment. expected {}, but {} is given",
                    format!("{:?}", s.r#type).green(),
                    format!("{:?}", r#type).red()
                ));
                ByteCode::Literal { value: ConstExpr::None }
            }
            Some(s) => {
                self.diagnostics.report(format!(
                    "cannot assign to immutable variable {}",
                    identifier_token.text
                ));
                ByteCode::Literal { value: ConstExpr::None }
            }
            None => {
                self.diagnostics.report(format!(
                    "undefined variable {}",
                    identifier_token.text
                ));
                ByteCode::Literal { value: ConstExpr::None }
            }
        }
    }

    fn check_block(&self, block: Block) -> ByteCode {
        let crate::analyze::syntax_tree::Block { expressions } = block;
        let expressions = expressions.borrow().iter().map(|e| self.check_expression(e.clone())).collect();
        ByteCode::Block { expressions }
    }

    fn check_binary_expression(&self, left: Box<Expression>, operator_token: Token, right: Box<Expression>) -> ByteCode {
        let checked_left = self.check_expression(*left);
        let checked_right = self.check_expression(*right);
        let left_type = self.type_of(&checked_left);
        let right_type = self.type_of(&checked_right);
        let op = BinaryOperator::check(operator_token.token_type, &left_type, &right_type);
        match op {
            Some(op) => ByteCode::Binary { op, left: Box::new(checked_left), right: Box::new(checked_right) },
            None => {
                self.diagnostics.report(format!(
                    "operator {} is not supported for types {} and {}",
                    operator_token.text.red(),
                    format!("{:?}", left_type).red(),
                    format!("{:?}", right_type).red()
                ));
                checked_left
            }
        }
    }

    fn check_var_declaration_expression(&self, declaration_token: Token, identifier_token: Token, equals_token: Option<Token>, assigment_expr: Option<Box<Expression>>) -> ByteCode {
        match assigment_expr {
            Some(e) => {
                let checked_expression = self.check_expression(*e);
                let mutable = declaration_token.token_type == VarKeyword;

                self.scope.set_local(&identifier_token.text, VariableSymbol::init_as(mutable, self.type_of(&checked_expression.clone()).clone(), ));

                Declaration { identifier: identifier_token, expression: Box::new(checked_expression) }
            }
            None => {
                self.scope.set_local(&identifier_token.text, VariableSymbol::new_immut(RawType::None, ));
                ByteCode::Literal { value: ConstExpr::None }
            }
        }
    }
    fn check_function_type_expression(&self, parameter_types: Vec<Expression>, return_type: Box<Expression>) -> ByteCode {
        let parameter_types: Vec<RawType> = parameter_types.iter().map(|p| self.check_expression(p.clone())).map(|p| self.type_of(&p)).collect();
        let checked_res_type = self.check_expression(*return_type);
        let res_type = self.type_of(&checked_res_type);

        FunType {
            _type: FunctionType {
                param_types: parameter_types,
                return_type: Box::from(res_type)
            },
        }
    }

    fn check_identifier_expression(&self, identifier: Token) -> ByteCode {
        let symbol = self.scope.get_global(&identifier.text);
        let r#type = match symbol {
            Some(s) => s.r#type,
            None => RawType::None,
        };
        ByteCode::Identifier { identifier }
    }


    fn check_unary_expression(&self, operator_token: Token, operand: Box<Expression>) -> ByteCode {
        let checked_operand = self.check_expression(*operand);
        let operand_type = self.type_of(&checked_operand);
        let op = UnaryOperator::check(operator_token.token_type, &operand_type);
        match op {
            Some(op) => ByteCode::Unary { op, operand: Box::new(checked_operand) },
            None => {
                self.diagnostics.report(format!(
                    "operator {} is not supported for type {}",
                    operator_token.text.red(),
                    format!("{:?}", operand_type).red()
                ));
                checked_operand
            }
        }
    }

    fn check_function_declaration(&self,
                                  identifier: Token,
                                  params: Vec<IdentifierTypePair>,
                                  res_type_description: Box<Expression>,
                                  body: Box<Expression>
    ) -> ByteCode {
        let parameter_types: Vec<RawType> = params.iter().map(|p| self.check_expression(p.clone().type_description)).map(|p| self.type_of(&p)).collect();


        let child_scope = self.new_child();
        for (i, param) in params.iter().enumerate() {
            child_scope.scope.set_local(&param.name.text, VariableSymbol::new_immut(parameter_types[i].clone(), ));
        }
        let checked_body = child_scope.check_expression(*body);// should be a Block
        let body_type = child_scope.type_of(&checked_body);
        self.diagnostics.append(child_scope.diagnostics.clone());

        let checked_res_type = self.check_expression(*res_type_description);
        let res_type = self.type_of(&checked_res_type);

        self.match_function_body_type(&*identifier.text, &checked_body, res_type.clone());

        let _type = FunctionType {
            param_types: parameter_types.clone(),
            return_type: Box::from(res_type.clone())
        };

        self.scope.set_local(&identifier.text, VariableSymbol::init_as_immut(RawType::FunctionType { _type: _type.clone() }, ));

        ByteCode::FunctionDeclaration {
            name: identifier,
            function: FunctionObj {
                _type: FunctionType {
                    param_types: parameter_types,
                    return_type: Box::from(res_type)
                },
                param_names: params.iter().map(|p| p.name.text.clone()).collect(),
                body: Box::new(checked_body)
            }
        }
    }

    fn check_func_call(&self, identifier: Token, arguments: Vec<Expression>) -> ByteCode {
        if BUILT_IN_FUNCTION_NAME.contains(&identifier.text) {
            let arguments: Vec<ByteCode> = arguments.iter().map(|a| self.check_expression(a.clone())).collect();
            ByteCode::CallBuiltIn { name: identifier, arguments }
        } else {
            match self.scope.get_global(&identifier.text) {
                Some(VariableSymbol { r#type, .. }) => match r#type {
                    RawType::FunctionType { _type } => if _type.param_types.len() == arguments.len() {
                        let child_scope = self.new_child();
                        let arguments: Vec<ByteCode> = arguments.iter().map(|a| child_scope.check_expression(a.clone())).collect();
                        for (i, arg) in arguments.iter().enumerate() {
                            let arg_type = self.type_of(&arg);
                            if arg_type != _type.param_types[i] {
                                self.diagnostics.report(format!(
                                    "the argument {} type is not matched in function {}. need {}, but {} is given",
                                    i.to_string().red(),
                                    identifier.text.blue(),
                                    format!("{:?}", _type.param_types[i]).green(),
                                    format!("{:?}", arg_type).red()
                                ));
                            }
                        }

                        ByteCode::Call { name: identifier, function: FunctionType { param_types: _type.param_types, return_type: _type.return_type }, arguments }
                    } else {
                        self.diagnostics.report_argument_count_mismatch(&identifier.text, _type.param_types.len(), arguments.len());
                        ByteCode::Literal { value: ConstExpr::None }
                    },
                    _ => {
                        self.diagnostics.report(format!("{} not a function", identifier.text));
                        ByteCode::Literal { value: ConstExpr::None }
                    }
                },
                None => {
                    self.diagnostics.report_undefined_function(&identifier.text);
                    ByteCode::Literal { value: ConstExpr::None }
                }
            }
        }
    }


    pub fn type_of(&self, expression: &ByteCode) -> RawType {
        match expression {
            ByteCode::Statement { .. } => { RawType::None }
            ByteCode::Literal { value } => match value {
                ConstExpr::I32(_) => I32,
                ConstExpr::F32(_) => F32,
                ConstExpr::Bool(_) => Bool,
                ConstExpr::None => RawType::None,
                ConstExpr::Function { fun } => *fun._type.return_type.clone(),
            },
            ByteCode::Unary { op, .. } => op.res_type.clone(),
            ByteCode::Binary { op, .. } => op.res_type.clone(),
            ByteCode::Block { expressions } => expressions.last().map(|e| self.type_of(e)).unwrap_or(RawType::None),
            ByteCode::Identifier { identifier } => {
                match self.check_base_type(&identifier.text) {
                    Some(t) => t,
                    None => {
                        let symbol = self.scope.get_global(&identifier.text);
                        match symbol {
                            Some(s) => s.r#type,
                            None => {
                                self.diagnostics.report_undefined_variable(&identifier.text);
                                RawType::None
                            }
                        }
                    }
                }
            }
            ByteCode::Assignment { .. } => RawType::None,
            ByteCode::If { then, .. } => self.type_of(then),
            ByteCode::Loop { .. } => RawType::None,
            ByteCode::While { .. } => RawType::None,
            Break => RawType::None,
            ByteCode::Continue => RawType::None,
            ByteCode::FunctionDeclaration { name, function } => RawType::FunctionType { _type: function._type.clone() },
            ByteCode::Call { name, function: function_obj, arguments } => {
                let fun = self.scope.get_global(&name.text);
                match fun {
                    Some(s) => {
                        match s.r#type {
                            RawType::FunctionType { _type } => *_type.return_type,
                            _ => {
                                self.diagnostics.report(format!("{} is not a function", name.text));
                                RawType::None
                            }
                        }
                    }
                    None => {
                        self.diagnostics.report_undefined_function(&name.text);
                        RawType::None
                    }
                }
            }
            ByteCode::CallBuiltIn { name, arguments } => {
                RawType::None
            }
            ByteCode::Return { expression } => self.type_of(expression),
            FunType { _type } => {
                RawType::FunctionType { _type: _type.clone() }
            }
            Declaration => { RawType::None }
        }
    }


    fn check_return_type(&self, name: &str, expression: &ByteCode, res_type: RawType) {
        if self.type_of(expression) != res_type {
            self.diagnostics.report(format!("return type mismatch in fun {}", name.red()));
        }
    }

    fn match_function_body_type(&self, name: &str, body: &ByteCode, res_type: RawType) {
        if let ByteCode::Block { expressions } = body {
            for e in expressions {
                if let ByteCode::Return { expression } = e {
                    self.check_return_type(name, expression, res_type.clone());
                }
            }
            if let Some(e) = expressions.last() {
                self.check_return_type(name, e, res_type);
            }
        }
    }

    fn check_base_type(&self, name: &str) -> Option<RawType> {
        match name {
            "I32" => Some(I32),
            "F32" => Some(F32),
            "Bool" => Some(Bool),
            _ => None
        }
    }
}








