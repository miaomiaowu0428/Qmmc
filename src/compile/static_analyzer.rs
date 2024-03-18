use std::rc::Rc;

use colored::Colorize;
use lazy_static::lazy_static;
use RawType::Unit;

use TokenType::VarKeyword;

use crate::analyze::diagnostic::DiagnosticBag;
use crate::analyze::lex::{Token, TokenType};
use crate::analyze::parse::{Block, Expression, IdentifierTypePair};
use crate::compile::binary_operator::BinaryOperator;
use crate::compile::checked_expression::{CheckedExpression, ConstExpr};
use crate::compile::compile_time_scope::CompileTimeScope;
use crate::compile::r#type::FunctionType;
use crate::compile::unary_operator::UnaryOperator;
use crate::compile::variable_symbol::VariableSymbol;
use crate::compile::RawType::{Bool, F32, I32};
use crate::compile::{FunctionDeclare, RawType};

lazy_static! {
    static ref BUILT_IN_FUNCTION_NAME: Vec<String> =
        vec!["print".to_string(), "println".to_string()];
}

pub struct StaticAnalyzer {
    pub scope: Rc<CompileTimeScope>,
    pub diagnostics: DiagnosticBag,
}

impl StaticAnalyzer {
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

    pub fn analyse(&self, expressions: Vec<Expression>) -> Vec<CheckedExpression> {
        expressions
            .iter()
            .map(|e| self.check_expression(e.clone()))
            .collect()
    }
    pub fn check_expression(&self, expression: Expression) -> CheckedExpression {
        match expression {
            Expression::Statement { .. } => CheckedExpression::Statement {
                expression: Box::new(self.check_expression(expression.clone())),
            },
            Expression::LiteralExpression { literal_token } => CheckedExpression::Literal {
                value: ConstExpr::from(literal_token),
            },
            Expression::IdentifierExpression { identifier_token } => {
                self.check_identifier_expression(identifier_token)
            }
            Expression::UnaryExpression {
                operator_token,
                operand,
            } => self.check_unary_expression(operator_token, operand),
            Expression::BinaryExpression {
                left,
                operator_token,
                right,
            } => self.check_binary_expression(left, operator_token, right),
            Expression::BracketedExpression { block, .. } => self.check_block(block),
            Expression::ParenthesizedExpression { expression, .. } => {
                self.check_expression(*expression)
            }
            Expression::VarDeclarationExpression {
                declaration_token,
                identifier_token,
                equals_token,
                assigment_expr,
            } => self.check_var_declaration_expression(
                declaration_token,
                identifier_token,
                equals_token,
                assigment_expr,
            ),
            Expression::AssignmentExpression {
                identifier_token,
                expression,
                ..
            } => self.check_assignment_expression(&identifier_token, expression),
            Expression::ConditionalBranchExpression {
                if_expr,
                else_if_blocks,
                else_expr,
            } => self.check_conditional_branch_expression(if_expr, else_if_blocks, else_expr),
            Expression::WhileExpression {
                while_token,
                condition,
                body,
                ..
            } => self.check_while_expression(while_token, condition, body),
            Expression::LoopExpression { body, .. } => self.check_loop_expression(body),
            Expression::BreakExpression { .. } => CheckedExpression::Break,
            Expression::ContinueExpression { .. } => CheckedExpression::Continue,
            Expression::FunctionDeclarationExpression {
                identifier_token,
                parameters,
                res_type_description,
                body,
                ..
            } => self.check_function_declaration(
                identifier_token,
                parameters,
                res_type_description,
                body,
            ),
            Expression::ReturnExpression { expression, .. } => {
                self.check_return_expression(expression)
            }
            Expression::FunctionCallExpression {
                identifier_token,
                arguments,
                ..
            } => self.check_function_call(identifier_token, arguments),
            Expression::FunctionTypeExpression {
                parameter_types,
                return_type,
                ..
            } => self.check_function_type_expression(parameter_types, return_type),
            _ => {
                todo!("{}", format!("{:#?} not implemented yet", expression))
            }
        }
    }

    fn check_conditional_branch_expression(
        &self,
        if_expr: Box<Expression>,
        else_if_blocks: Vec<Expression>,
        else_expr: Option<Box<Expression>>,
    ) -> CheckedExpression {
        if let Expression::IfExpression {
            if_token,
            condition,
            then_block,
        } = *if_expr.clone()
        {
            let checked_condition = self.check_expression(*condition);
            if self.type_of(&checked_condition.clone()) != Bool {
                self.diagnostics.report(format!(
                    "{} at ({},{}): condition is not a boolean in if expression",
                    "Error".red(),
                    if_token.line_num,
                    if_token.column_num
                ));
            }
            let checked_then = self.check_expression(*then_block);
            let if_type = self.type_of(&checked_then);
            self.check_return_type(
                &format!("{}", &if_expr).as_str(),
                &checked_then,
                if_type.clone(),
            );

            let mut checked_else_ifs = Vec::new();
            for else_if_block in else_if_blocks {
                if let Expression::ElseIfExpression {
                    else_token,
                    if_token,
                    condition,
                    then_block,
                } = else_if_block
                {
                    let checked_condition = Box::from(self.check_expression(*condition));
                    if self.type_of(&checked_condition.clone()) != Bool {
                        self.diagnostics.report(format!(
                            "{} at ({},{}): condition is not a boolean in if expression\n{}",
                            "Error".red(),
                            if_token.line_num,
                            if_token.column_num,
                            format!("{:?}", checked_condition).red()
                        ));
                    }
                    let checked_then = Box::from(self.check_expression(*then_block));
                    let then_type = self.type_of(&checked_then);
                    if then_type != if_type {
                        self.diagnostics.report(format!("{} at ({},{}): type mismatch in if expression\nneed:{} but given {} \nexpr: {}", "Error".red(), if_token.line_num.to_string().red(), if_token.column_num.to_string().red(), format!("{:?}", if_type).green(), format!("{:?}", then_type).red(), format!("{:#?}", checked_then).red(), ));
                    }
                    checked_else_ifs.push(CheckedExpression::ElseIf {
                        condition: checked_condition,
                        body: checked_then,
                    });
                } else {
                    panic!("{:#?},not an else if expression", else_if_block);
                }
            }

            let checked_else = match else_expr {
                Some(else_expr) => {
                    if let Expression::ElseExpression { else_token, block } = *else_expr {
                        let checked_else_block = self.check_expression(*block);
                        if self.type_of(&checked_else_block) != if_type {
                            self.diagnostics.report(format!("{} at ({},{}): type mismatch in if expression\nneed:{} but given {} \nexpr: {}",
                                                            "Error".red(),
                                                            else_token.line_num.to_string().red(),
                                                            else_token.column_num.to_string().red(),
                                                            format!("{:?}", if_type).green(),
                                                            format!("{:?}", self.type_of(&checked_else_block)).red(),
                                                            format!("{:#?}", checked_else_block).red(),
                            ));
                        }
                        Some(Box::from(CheckedExpression::Else {
                            body: Box::from(checked_else_block),
                        }))
                    } else {
                        panic!("{} not a else expression", else_expr)
                    }
                }
                None => None,
            };

            CheckedExpression::Conditional {
                condition: Box::from(checked_condition),
                then: Box::from(checked_then),
                else_ifs: checked_else_ifs,
                else_expr: checked_else,
                _type: if_type,
            }
        } else {
            panic!("{:#?},not an if expression", if_expr);
        }
    }

    fn check_return_expression(&self, expression: Box<Expression>) -> CheckedExpression {
        let checked_expression = self.check_expression(*expression);
        CheckedExpression::Return {
            expression: Box::new(checked_expression),
        }
    }

    fn check_loop_expression(&self, body: Box<Expression>) -> CheckedExpression {
        let checked_body = self.check_expression(*body);
        CheckedExpression::Loop {
            body: Box::new(checked_body),
        }
    }

    fn check_while_expression(
        &self,
        while_token: Token,
        condition: Box<Expression>,
        body: Box<Expression>,
    ) -> CheckedExpression {
        let checked_condition = self.check_expression(*condition);
        let checked_body = self.check_expression(*body);
        if self.type_of(&checked_condition.clone()) != Bool {
            self.diagnostics.report(format!(
                "{} at ({},{}): condition is not a boolean in while expression",
                "Error".red(),
                while_token.line_num,
                while_token.column_num
            ));
        }
        CheckedExpression::While {
            condition: Box::new(checked_condition),
            body: Box::new(checked_body),
        }
    }

    fn check_assignment_expression(
        &self,
        identifier_token: &Token,
        expression: Box<Expression>,
    ) -> CheckedExpression {
        let checked_expression = self.check_expression(*expression);
        let r#type = self.type_of(&checked_expression);
        let symbol = self.scope.get_global(&identifier_token.text);
        match symbol {
            Some(s) if s.r#type == r#type && s.mutable => CheckedExpression::Assignment {
                identifier: identifier_token.clone(),
                expression: Box::new(checked_expression),
            },
            Some(s) if s.r#type == r#type && s.mutable == false && s.initialized == false => {
                self.scope.try_set_global(
                    &identifier_token.text,
                    VariableSymbol::init_as_immut(r#type.clone()),
                );
                CheckedExpression::Assignment {
                    identifier: identifier_token.clone(),
                    expression: Box::new(checked_expression),
                }
            }
            Some(s) if s.r#type != r#type && s.mutable => {
                self.diagnostics.report(format!(
                    "type mismatch in assignment. expected {}, but {} is given",
                    format!("{:?}", s.r#type).green(),
                    format!("{:?}", r#type).red()
                ));
                CheckedExpression::Literal {
                    value: ConstExpr::None,
                }
            }
            Some(s) => {
                self.diagnostics.report(format!(
                    "cannot assign to immutable variable {}",
                    identifier_token.text
                ));
                CheckedExpression::Literal {
                    value: ConstExpr::None,
                }
            }
            None => {
                self.diagnostics
                    .report(format!("undefined variable {}", identifier_token.text));
                CheckedExpression::Literal {
                    value: ConstExpr::None,
                }
            }
        }
    }

    fn check_block(&self, block: Block) -> CheckedExpression {
        let Block { expressions } = block;
        let expressions = expressions
            .borrow()
            .iter()
            .map(|e| self.check_expression(e.clone()))
            .collect();
        CheckedExpression::Block { expressions }
    }

    fn check_binary_expression(
        &self,
        left: Box<Expression>,
        operator_token: Token,
        right: Box<Expression>,
    ) -> CheckedExpression {
        let checked_left = self.check_expression(*left);
        let checked_right = self.check_expression(*right);
        let left_type = self.type_of(&checked_left);
        let right_type = self.type_of(&checked_right);
        let op = BinaryOperator::check(operator_token.token_type, &left_type, &right_type);
        match op {
            Some(op) => CheckedExpression::Binary {
                op,
                left: Box::new(checked_left),
                right: Box::new(checked_right),
            },
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

    fn check_var_declaration_expression(
        &self,
        declaration_token: Token,
        identifier_token: Token,
        equals_token: Option<Token>,
        assigment_expr: Option<Box<Expression>>,
    ) -> CheckedExpression {
        match assigment_expr {
            Some(e) => {
                let checked_expression = self.check_expression(*e);
                let mutable = declaration_token.token_type == VarKeyword;
                let _type = self.type_of(&checked_expression.clone()).clone();

                self.scope.set_local(
                    &identifier_token.text,
                    VariableSymbol::init_as(mutable, _type.clone()),
                );

                CheckedExpression::VarDeclare {
                    name: identifier_token,
                    init_expr: Box::new(checked_expression),
                    _type,
                }
            }
            None => {
                self.scope
                    .set_local(&identifier_token.text, VariableSymbol::new_immut(Unit));
                CheckedExpression::Literal {
                    value: ConstExpr::None,
                }
            }
        }
    }
    fn check_function_type_expression(
        &self,
        parameter_types: Vec<Expression>,
        return_type: Box<Expression>,
    ) -> CheckedExpression {
        let parameter_types: Vec<RawType> = parameter_types
            .iter()
            .map(|p| self.check_expression(p.clone()))
            .map(|p| self.type_of(&p))
            .collect();
        let checked_res_type = self.check_expression(*return_type);
        let res_type = self.type_of(&checked_res_type);

        CheckedExpression::FunType {
            _type: FunctionType {
                param_types: parameter_types,
                return_type: Box::from(res_type),
            },
        }
    }

    fn check_identifier_expression(&self, identifier: Token) -> CheckedExpression {
        let symbol = self.scope.get_global(&identifier.text);
        let r#type = match symbol {
            Some(s) => s.r#type,
            None => Unit,
        };
        CheckedExpression::Identifier { name: identifier }
    }

    fn check_unary_expression(
        &self,
        operator_token: Token,
        operand: Box<Expression>,
    ) -> CheckedExpression {
        let checked_operand = self.check_expression(*operand);
        let operand_type = self.type_of(&checked_operand);
        let op = UnaryOperator::check(operator_token.token_type, &operand_type);
        match op {
            Some(op) => CheckedExpression::Unary {
                op,
                operand: Box::new(checked_operand),
            },
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

    fn check_function_declaration(
        &self,
        identifier: Token,
        params: Vec<IdentifierTypePair>,
        res_type_description: Box<Expression>,
        body: Box<Expression>,
    ) -> CheckedExpression {
        let parameter_types: Vec<RawType> = params
            .iter()
            .map(|p| self.check_expression(p.clone().type_description))
            .map(|p| self.type_of(&p))
            .collect();
        let checked_res_type = self.check_expression(*res_type_description);

        //先把自己的声明添加到现在的scope里
        //为了避免递归调用时找不到自己的声明, 先声明一个空的函数
        //之后应该还是要处理成先把所有的函数都声明, 然后再处理函数体
        // todo!();
        self.scope.declare_function(
            &identifier.text,
            FunctionDeclare {
                _type: FunctionType {
                    param_types: parameter_types.clone(),
                    return_type: Box::from(self.type_of(&checked_res_type)),
                },
                param_names: params.iter().map(|p| p.name.text.clone()).collect(),
                body: CheckedExpression::Literal {
                    value: ConstExpr::None,
                },
            },
        );

        let child_scope = self.new_child();
        for (i, param) in params.iter().enumerate() {
            child_scope.scope.set_local(
                &param.name.text,
                VariableSymbol::new_mut(parameter_types[i].clone()),
            );
        }
        let checked_body = child_scope.check_function_body(
            &*identifier.text,
            body,
            child_scope.type_of(&checked_res_type),
        );
        self.diagnostics.append(child_scope.diagnostics.clone());

        let res_type = self.type_of(&checked_res_type);

        let _type = FunctionType {
            param_types: parameter_types.clone(),
            return_type: Box::from(res_type.clone()),
        };

        let func_declare = FunctionDeclare {
            _type: _type.clone(),
            param_names: params.iter().map(|p| p.name.text.clone()).collect(),
            body: checked_body.clone(),
        };

        self.scope
            .declare_function(&identifier.text, func_declare.clone());

        CheckedExpression::FunctionDeclaration {
            name: identifier,
            function: Box::from(func_declare),
        }
    }

    fn check_function_body(
        &self,
        name: &str,
        body: Box<Expression>,
        res_type: RawType,
    ) -> CheckedExpression {
        let mut expressions: Vec<CheckedExpression> = vec![];
        if let Expression::BracketedExpression {
            left_b,
            block,
            right_b,
        } = *body
        {
            for e in block.expressions.borrow().iter() {
                let checked_expression = self.check_expression(e.clone());
                if let CheckedExpression::Return { expression } = &checked_expression {
                    self.check_return_type(name, &expression, res_type.clone());
                } else {
                    self.check_inner_return_type(name, &checked_expression, res_type);
                }
                expressions.push(checked_expression);
            }
            if let Some(e) = expressions.last() {
                self.check_return_type(name, e, res_type);
            }
        }
        CheckedExpression::Block { expressions }
    }

    fn check_inner_return_type(
        &self,
        name: &str,
        expression: &CheckedExpression,
        res_type: RawType,
    ) {
        match expression {
            CheckedExpression::Block { expressions } => {
                for e in expressions {
                    self.check_inner_return_type(name, e, res_type.clone());
                }
            }
            CheckedExpression::Return { expression } => {
                self.check_return_type(name, expression, res_type);
            }
            CheckedExpression::If {
                condition,
                body: then,
                r#else,
            } => {
                self.check_inner_return_type(name, then, res_type.clone());
                if let Some(e) = r#else {
                    self.check_inner_return_type(name, e, res_type);
                }
            }
            CheckedExpression::Loop { body } => {
                self.check_inner_return_type(name, body, res_type);
            }
            CheckedExpression::While { condition, body } => {
                self.check_inner_return_type(name, body, res_type);
            }
            _ => {}
        }
    }

    fn check_function_call(&self, name: Token, arguments: Vec<Expression>) -> CheckedExpression {
        if BUILT_IN_FUNCTION_NAME.contains(&name.text) {
            let arguments: Vec<CheckedExpression> = arguments
                .iter()
                .map(|a| self.check_expression(a.clone()))
                .collect();
            CheckedExpression::CallBuiltIn { name, arguments }
        } else {
            let fun = self.scope.get_global_function(&name.text);
            match fun {
                Some(f) => {
                    let arguments: Vec<CheckedExpression> = arguments
                        .iter()
                        .map(|a| self.check_expression(a.clone()))
                        .collect();
                    if f._type.param_types.len() != arguments.len() {
                        self.diagnostics.report(format!(
                            "function {} expects {} arguments, but {} given",
                            name.text.red(),
                            f._type.param_types.len().to_string().green(),
                            arguments.len().to_string().red()
                        ));
                    }
                    for (i, arg) in arguments.iter().enumerate() {
                        if f._type.param_types[i] != self.type_of(arg) {
                            self.diagnostics.report(format!(
                                "type mismatch in argument {}. expected {}, but {} given",
                                i.to_string().green(),
                                format!("{:?}", f._type.param_types[i]).green(),
                                format!("{:?}", self.type_of(arg)).red()
                            ));
                        }
                    }
                    CheckedExpression::Call {
                        name,
                        function: f._type.clone(),
                        arguments,
                    }
                }
                None => {
                    self.diagnostics.report(format!(
                        "function {} called at ({},{}) is not defined",
                        name.text.red(),
                        name.line_num.to_string().red(),
                        name.column_num.to_string().red()
                    ));
                    CheckedExpression::Literal {
                        value: ConstExpr::None,
                    }
                }
            }
        }
    }

    pub fn type_of(&self, expression: &CheckedExpression) -> RawType {
        match expression {
            CheckedExpression::Statement { .. } => Unit,
            CheckedExpression::Literal { value } => match value {
                ConstExpr::I32(_) => I32,
                ConstExpr::F32(_) => F32,
                ConstExpr::Bool(_) => Bool,
                ConstExpr::None => Unit,
            },
            CheckedExpression::Unary { op, .. } => op.res_type.clone(),
            CheckedExpression::Binary { op, .. } => op.res_type.clone(),
            CheckedExpression::Block { expressions } => {
                expressions.last().map(|e| self.type_of(e)).unwrap_or(Unit)
            }
            CheckedExpression::Identifier { name: identifier } => {
                match self.check_base_type(&identifier.text) {
                    Some(t) => t,
                    None => {
                        let symbol = self.scope.get_global(&identifier.text);
                        match symbol {
                            Some(s) => s.r#type,
                            None => {
                                self.diagnostics.report(format!(
                                    "{} at ({},{}) is not defined",
                                    identifier.text.red(),
                                    identifier.line_num.to_string().red(),
                                    identifier.column_num.to_string().red()
                                ));
                                Unit
                            }
                        }
                    }
                }
            }
            CheckedExpression::Assignment { .. } => Unit,
            CheckedExpression::Conditional { then, .. } => self.type_of(then),
            CheckedExpression::If { body, .. } => self.type_of(body),
            CheckedExpression::ElseIf { body, .. } => self.type_of(body),
            CheckedExpression::Else { body } => self.type_of(body),
            CheckedExpression::Loop { .. } => Unit,
            CheckedExpression::While { .. } => Unit,
            CheckedExpression::Break => Unit,
            CheckedExpression::Continue => Unit,
            CheckedExpression::FunctionDeclaration { name, function } => {
                todo!("get fun and return the type of the fun")
            }
            CheckedExpression::Call {
                name,
                function: function_obj,
                arguments,
            } => {
                if let Some(fun) = self.scope.get_global_function(&name.text) {
                    *fun._type.return_type
                } else {
                    Unit
                }
            }
            CheckedExpression::CallBuiltIn { name, arguments } => Unit,
            CheckedExpression::Return { expression } => Unit,
            CheckedExpression::FunType { _type } => {
                FunctionType {
                    param_types: _type.param_types.clone(),
                    return_type: _type.return_type.clone(),
                };
                Unit
            }
            CheckedExpression::VarDeclare { .. } => Unit,
        }
    }

    fn check_return_type(&self, name: &str, expression: &CheckedExpression, res_type: RawType) {
        if self.type_of(expression) != res_type {
            self.diagnostics.report(format!(
                "return type mismatch in fun {}, expected {}, given {}",
                name.red(),
                format!("{:?}", res_type).green(),
                format!("{:?}", self.type_of(expression)).red()
            ));
        }
    }

    fn check_base_type(&self, name: &str) -> Option<RawType> {
        match name {
            "I32" => Some(I32),
            "F32" => Some(F32),
            "Bool" => Some(Bool),
            "()" => Some(Unit),
            "Unit" => Some(Unit),
            _ => None,
        }
    }
}
