use std::cell::RefCell;

use colored::Colorize;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::{FloatPredicate, IntPredicate};
use inkwell::module::Module;
use inkwell::values::{BasicMetadataValueEnum, PointerValue};
use inkwell::values::BasicValue;
use inkwell::values::BasicValueEnum;
use inkwell::values::FunctionValue;

use crate::analyze::diagnostic::DiagnosticBag;
use crate::analyze::lex::Token;
use crate::compile::{
    BinaryOperatorType, CheckedExpression, ConstExpr, FunctionDeclare, FunctionType, RawType,
    UnaryOperatorType,
};
use crate::IR_building::loop_info::{LoopGuard, LoopStack};
use crate::IR_building::symbol_table::{ScopeGuard, SymbolTable};

pub struct IRBuilder<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    current_function: RefCell<Option<FunctionValue<'ctx>>>,
    symbol_table: SymbolTable<'ctx>,
    loop_stack: LoopStack<'ctx>,
    pub(crate) diagnostics: DiagnosticBag,
}

impl<'ctx> IRBuilder<'ctx> {
    pub fn new(
        context: &'ctx Context,
        module: Module<'ctx>,
        builder: Builder<'ctx>,
    ) -> IRBuilder<'ctx> {
        IRBuilder {
            context,
            module,
            builder,
            current_function: RefCell::new(None),
            symbol_table: SymbolTable::new(),
            loop_stack: LoopStack::new(),
            diagnostics: DiagnosticBag::new(),
        }
    }

    pub fn save_as(&self, path: &str) {
        let path = std::path::Path::new(path);
        match self.module.print_to_file(&path) {
            Ok(_) => println!(
                "{}",
                format!("{:<26}: {}", "Successfully compiled to", path.display()).green()
            ),
            Err(e) => self
                .diagnostics
                .report(format!("Failed to write to file: {}", e.to_string())),
        }
    }

    pub fn print_res(&self) {
        println!("{}", self.module.print_to_string().to_string());
    }

    pub fn build_irs(&self, expressions: Vec<CheckedExpression>) {
        for (i, expr) in expressions.iter().enumerate() {
            match expr {
                CheckedExpression::VarDeclare { name, init_expr } => {
                    self.build_ir(expr.clone());
                }
                CheckedExpression::FunctionDeclaration { name, function } => {
                    self.build_ir(expr.clone());
                }
                _ => {
                    self.diagnostics.report(format!(
                        "{} at expr({}): can only declare {} or {} in global found {:#?}",
                        "Error".to_string().red(),
                        i,
                        "function".to_string().green(),
                        "variable".to_string().green(),
                        expr
                    ));
                }
            }
        }
    }

    fn build_ir(&self, expression: CheckedExpression) {
        // println!("Current Block: {:#?}", self.builder.get_insert_block());
        // println!("Building: {:#?}", expression);

        match expression {
            CheckedExpression::FunctionDeclaration { name, function } => {
                self.build_function_declare(name, *function);
            }
            CheckedExpression::Return { expression } => {
                self.builder
                    .build_return(self.build_basic_value(*expression).as_deref())
                    .expect("build return failed");
            }
            CheckedExpression::Block { expressions } => {
                self.build_block(expressions);
            }
            CheckedExpression::Conditional {
                condition,
                then,
                else_ifs,
                else_expr, _type,
            } => {
                self.build_conditional(condition, then, else_ifs, else_expr);
            }
            CheckedExpression::VarDeclare { name, init_expr } => {
                let var = self
                    .builder
                    .build_alloca(self.context.i32_type(), &name.text)
                    .expect("build alloc failed");
                self.symbol_table.insert(name.text.clone(), var);
                self.builder
                    .build_store(
                        var,
                        self.build_basic_value(*init_expr)
                            .unwrap()
                            .as_basic_value_enum()
                            .into_int_value(),
                    )
                    .expect("build store failed");
            }
            CheckedExpression::Assignment {
                identifier: name,
                expression,
            } => {
                let var = self.symbol_table.get(&name.text).unwrap();
                let res = self
                    .build_basic_value(*expression)
                    .unwrap()
                    .as_basic_value_enum()
                    .into_int_value();
                self.builder
                    .build_store(var, res)
                    .expect("build store failed");
            }
            CheckedExpression::While { condition, body } => {
                self.build_while(self.current_function.borrow().unwrap(), condition, body);
            }
            CheckedExpression::Loop { body } => {
                self.build_loop(*body);
            }
            CheckedExpression::Break => {
                let merge_block = self.loop_stack.top().unwrap().merge_block;
                self.builder
                    .build_unconditional_branch(merge_block)
                    .expect("build unconditional branch failed");
            }
            CheckedExpression::Continue => {
                let body_block = self.loop_stack.top().unwrap().body_block;
                self.builder
                    .build_unconditional_branch(body_block)
                    .expect("build unconditional branch failed");
            }
            CheckedExpression::Call {
                name,
                function,
                arguments,
            } => {
                self.value_of_call(name, function, arguments);
            }

            _ => todo!(
                "\n{}{:#?}",
                "cannot build instruction from Expression not implemented: \n".red(),
                expression
            ),
        }

        // println!("Current Block: {:#?}", self.builder.get_insert_block());
    }

    fn build_loop(&self, body: CheckedExpression) {
        if let CheckedExpression::Block { expressions } = body {
            let _guard = ScopeGuard::new(&self.symbol_table);

            let loop_block = self
                .context
                .append_basic_block(self.current_function.borrow().unwrap(), "loop");
            let merge_block = self
                .context
                .append_basic_block(self.current_function.borrow().unwrap(), "merge");

            let _guard = LoopGuard::new(&self.loop_stack, merge_block, loop_block);

            self.builder
                .build_unconditional_branch(loop_block)
                .expect("build unconditional branch failed");

            self.builder.position_at_end(loop_block);
            for e in expressions {
                self.build_ir(e);
            }
            self.builder
                .build_unconditional_branch(loop_block)
                .expect("build unconditional branch failed");

            self.builder.position_at_end(merge_block);
        }
    }

    fn build_while(
        &self,
        function_value: FunctionValue<'ctx>,
        condition: Box<CheckedExpression>,
        body: Box<CheckedExpression>,
    ) {
        let _guard = ScopeGuard::new(&self.symbol_table);

        let condition_block = self.context.append_basic_block(function_value, "condition");
        let body_block = self.context.append_basic_block(function_value, "body");
        let merge_block = self.context.append_basic_block(function_value, "merge");

        let _guard = LoopGuard::new(&self.loop_stack, merge_block, body_block);

        self.builder
            .build_unconditional_branch(condition_block)
            .expect("build unconditional branch failed");

        self.builder.position_at_end(condition_block);
        let condition = self
            .build_basic_value(*condition)
            .unwrap()
            .as_basic_value_enum()
            .into_int_value();
        self.builder
            .build_conditional_branch(condition, body_block, merge_block)
            .expect("build conditional branch failed");

        self.builder.position_at_end(body_block);
        self.build_ir(*body);
        self.builder
            .build_unconditional_branch(condition_block)
            .expect("build unconditional branch failed");

        self.builder.position_at_end(merge_block);
    }

    fn build_function_declare(&self, name: Token, function: FunctionDeclare) {
        let LLVM_function_type = {
            let mut types = vec![];
            for param in function._type.param_types.clone() {
                match param {
                    RawType::I32 => types.push(self.context.i32_type().into()),
                    RawType::F32 => types.push(self.context.f32_type().into()),
                    RawType::Bool => types.push(self.context.bool_type().into()),
                    _ => todo!("{}", "cannot convert to function type".red()),
                }
            }
            let fun_type = match *function._type.return_type {
                RawType::I32 => self.context.i32_type().fn_type(&types, false),
                RawType::F32 => self.context.f32_type().fn_type(&types, false),
                RawType::Bool => self.context.bool_type().fn_type(&types, false),
                RawType::Unit => self.context.void_type().fn_type(&types, false),
            };
            fun_type
        };

        let LLVM_function_OBJ = self
            .module
            .add_function(&name.text, LLVM_function_type, None);
        *self.current_function.borrow_mut() = Some(LLVM_function_OBJ);

        let basic_block = self.context.append_basic_block(LLVM_function_OBJ, "entry");
        self.builder.position_at_end(basic_block);
        self.build_func_body(function);
    }

    fn build_func_body(&self, function_declare: FunctionDeclare) {
        let _guard = ScopeGuard::new(&self.symbol_table);

        for (i, param) in function_declare.param_names.iter().enumerate() {
            let arg = self
                .current_function
                .borrow()
                .unwrap()
                .get_nth_param(i as u32)
                .unwrap();
            arg.set_name(&param);
            let alloca = match function_declare._type.param_types[i] {
                RawType::I32 => self.builder.build_alloca(self.context.i32_type(), &param),
                RawType::F32 => self.builder.build_alloca(self.context.f32_type(), &param),
                RawType::Bool => self.builder.build_alloca(self.context.bool_type(), &param),
                T => todo!(
                    "{}",
                    format!("{}{:#?}{}", "cannot convert", T, " to function type").red()
                ),
            }
                .unwrap();
            self.builder
                .build_store(alloca, arg)
                .expect("build store failed");
            self.symbol_table.insert(param.clone(), alloca);
        }

        let body = function_declare.body;

        if let CheckedExpression::Block { expressions } = body {
            for e in expressions.iter().take(expressions.len() - 1) {
                self.build_ir(e.clone())
            }
            if let Some(last) = expressions.last() {
                match last {
                    CheckedExpression::Return { expression } => {
                        self.builder
                            .build_return(self.build_basic_value(*expression.clone()).as_deref())
                            .unwrap();
                    }
                    CheckedExpression::Conditional {
                        condition,
                        then,
                        else_ifs,
                        else_expr, _type,
                    } => {
                        self.build_valued_conditional(
                            condition.clone(),
                            then.clone(),
                            else_ifs.clone(),
                            else_expr.clone(),
                        );
                    }
                    _ => {
                        self.builder
                            .build_return(self.build_basic_value(last.clone()).as_deref())
                            .unwrap();
                    }
                }
            }
        } else {
            self.diagnostics
                .report("Function body must be a block".to_string());
        }
    }

    fn build_basic_value(&self, expression: CheckedExpression) -> Option<Box<dyn BasicValue + '_>> {
        match expression {
            CheckedExpression::Statement { expression } => {
                self.build_ir(*expression);
                None
            }
            CheckedExpression::Return { expression } => {
                self.builder
                    .build_return(self.build_basic_value(*expression).as_deref())
                    .expect("build return failed");
                None
            }
            CheckedExpression::Block { expressions } => {
                self.build_block_may_contains_value(expressions)
            }
            CheckedExpression::Identifier { name } => {
                let var = self.symbol_table.get(&name.text).unwrap();
                Some(Box::from(
                    self.builder
                        .build_load(PointerValue::try_from(var).unwrap(), &name.text)
                        .expect("build load failed"),
                ))
            }
            CheckedExpression::Literal { value } => match value {
                ConstExpr::I32(i) => {
                    let i32_type = self.context.i32_type();
                    let i32_value = i32_type.const_int(i as u64, false);
                    Some(Box::from(i32_value))
                }
                ConstExpr::Bool(b) => {
                    let bool_type = self.context.bool_type();
                    let bool_value = bool_type.const_int(b as u64, false);
                    Some(Box::from(bool_value))
                }
                ConstExpr::F32(f) => {
                    let f32_type = self.context.f32_type();
                    let f32_value = f32_type.const_float(f as f64);
                    Some(Box::from(f32_value))
                }
                ConstExpr::None => {
                    self.diagnostics.report("Invalid literal: None".to_string());
                    None
                }
            },
            CheckedExpression::Unary { op, operand } => match (op.operator_type, op.operand_type) {
                (UnaryOperatorType::Positive, RawType::I32)
                | (UnaryOperatorType::Positive, RawType::F32) => {
                    self.build_basic_value(*operand).or(None)
                }
                (UnaryOperatorType::Negation, RawType::I32) => {
                    let v = self.build_basic_value(*operand).unwrap();
                    if let BasicValueEnum::IntValue(i) = v.as_basic_value_enum() {
                        Some(Box::from(
                            self.builder
                                .build_int_neg(i, "neg")
                                .expect("build i32 negation failed"),
                        ))
                    } else {
                        self.diagnostics
                            .report(format!("'-' is not defined for {:?}", v));
                        None
                    }
                }
                (UnaryOperatorType::Negation, RawType::F32) => {
                    let v = self.build_basic_value(*operand).unwrap();
                    if let BasicValueEnum::FloatValue(f) = v.as_basic_value_enum() {
                        Some(Box::from(
                            self.builder
                                .build_float_neg(f, "neg")
                                .expect("build f32 negation failed"),
                        ))
                    } else {
                        self.diagnostics
                            .report(format!("'-' is not defined for {:?}", v));
                        None
                    }
                }
                (UnaryOperatorType::LogicalNegation, RawType::Bool) => {
                    let v = self.build_basic_value(*operand).unwrap();
                    if let BasicValueEnum::IntValue(i) = v.as_basic_value_enum() {
                        Some(Box::from(
                            self.builder
                                .build_not(i, "not")
                                .expect("build logical negation failed"),
                        ))
                    } else {
                        self.diagnostics
                            .report("'not' is not defined for non-integer types".to_string());
                        None
                    }
                }

                _ => todo!(
                    "{}{:#?}",
                    "Unary operator not implemented for ".red(),
                    operand
                ),
            }, // end of Unary
            CheckedExpression::Binary { op, left, right } => {
                match (op.left_type, op.right_type) {
                    (RawType::Bool, RawType::Bool) => {
                        let left = self.build_basic_value(*left).unwrap();
                        let right = self.build_basic_value(*right).unwrap();
                        let res = match op.operator_type {
                            BinaryOperatorType::Equals => {
                                let res = self.builder.build_int_compare(
                                    IntPredicate::EQ,
                                    left.as_basic_value_enum().into_int_value(),
                                    right.as_basic_value_enum().into_int_value(),
                                    "eq",
                                );
                                res.expect("build int compare failed")
                            }
                            BinaryOperatorType::NotEquals => {
                                let res = self.builder.build_int_compare(
                                    IntPredicate::NE,
                                    left.as_basic_value_enum().into_int_value(),
                                    right.as_basic_value_enum().into_int_value(),
                                    "ne",
                                );
                                res.expect("build int compare failed")
                            }
                            BinaryOperatorType::LogicalAnd => {
                                let res = self.builder.build_and(
                                    left.as_basic_value_enum().into_int_value(),
                                    right.as_basic_value_enum().into_int_value(),
                                    "and",
                                );
                                res.expect("build and failed")
                            }
                            BinaryOperatorType::LogicalOr => {
                                let res = self.builder.build_or(
                                    left.as_basic_value_enum().into_int_value(),
                                    right.as_basic_value_enum().into_int_value(),
                                    "or",
                                );
                                res.expect("build or failed")
                            }
                            BinaryOperatorType::GreaterThan => {
                                let res = self.builder.build_int_compare(
                                    IntPredicate::SGT,
                                    left.as_basic_value_enum().into_int_value(),
                                    right.as_basic_value_enum().into_int_value(),
                                    "gt",
                                );
                                res.expect("build int compare failed")
                            }
                            _ => todo!(
                                "{}: {:?} {:#?} {:?}",
                                "Binary operator not implemented for ".red(),
                                op.left_type,
                                op,
                                op.right_type
                            ),
                        };
                        Some(Box::from(res))
                    },
                    (RawType::I32, RawType::I32) => {
                        let left = self.build_basic_value(*left).unwrap();
                        let right = self.build_basic_value(*right).unwrap();
                        let res = match op.operator_type {
                            BinaryOperatorType::Addition => {
                                let res = self.builder.build_int_add(
                                    left.as_basic_value_enum().into_int_value(),
                                    right.as_basic_value_enum().into_int_value(),
                                    "add",
                                );
                                res.expect("build int add failed")
                            }
                            BinaryOperatorType::Subtraction => {
                                let res = self.builder.build_int_sub(
                                    left.as_basic_value_enum().into_int_value(),
                                    right.as_basic_value_enum().into_int_value(),
                                    "sub",
                                );
                                res.expect("build int sub failed")
                            }
                            BinaryOperatorType::Multiplication => {
                                let res = self.builder.build_int_mul(
                                    left.as_basic_value_enum().into_int_value(),
                                    right.as_basic_value_enum().into_int_value(),
                                    "mul",
                                );
                                res.expect("build int mul failed")
                            }
                            BinaryOperatorType::Division => {
                                let res = self.builder.build_int_signed_div(
                                    left.as_basic_value_enum().into_int_value(),
                                    right.as_basic_value_enum().into_int_value(),
                                    "div",
                                );
                                res.expect("build int div failed")
                            }
                            BinaryOperatorType::Remainder => {
                                let res = self.builder.build_int_signed_rem(
                                    left.as_basic_value_enum().into_int_value(),
                                    right.as_basic_value_enum().into_int_value(),
                                    "rem",
                                );
                                res.expect("build int rem failed")
                            }
                            BinaryOperatorType::Equals => {
                                let res = self.builder.build_int_compare(
                                    IntPredicate::EQ,
                                    left.as_basic_value_enum().into_int_value(),
                                    right.as_basic_value_enum().into_int_value(),
                                    "eq",
                                );
                                res.expect("build int compare failed")
                            }
                            BinaryOperatorType::NotEquals => {
                                let res = self.builder.build_int_compare(
                                    IntPredicate::NE,
                                    left.as_basic_value_enum().into_int_value(),
                                    right.as_basic_value_enum().into_int_value(),
                                    "ne",
                                );
                                res.expect("build int compare failed")
                            }
                            BinaryOperatorType::GreaterThan => {
                                let res = self.builder.build_int_compare(
                                    IntPredicate::SGT,
                                    left.as_basic_value_enum().into_int_value(),
                                    right.as_basic_value_enum().into_int_value(),
                                    "gt",
                                );
                                res.expect("build int compare failed")
                            }
                            BinaryOperatorType::LessThan => {
                                let res = self.builder.build_int_compare(
                                    IntPredicate::SLT,
                                    left.as_basic_value_enum().into_int_value(),
                                    right.as_basic_value_enum().into_int_value(),
                                    "lt",
                                );
                                res.expect("build int compare failed")
                            }
                            BinaryOperatorType::GreaterThanOrEqual => {
                                let res = self.builder.build_int_compare(
                                    IntPredicate::SGE,
                                    left.as_basic_value_enum().into_int_value(),
                                    right.as_basic_value_enum().into_int_value(),
                                    "ge",
                                );
                                res.expect("build int compare failed")
                            }
                            BinaryOperatorType::LessThanOrEqual => {
                                let res = self.builder.build_int_compare(
                                    IntPredicate::SLE,
                                    left.as_basic_value_enum().into_int_value(),
                                    right.as_basic_value_enum().into_int_value(),
                                    "le",
                                );
                                res.expect("build int compare failed")
                            }
                            _ => todo!(
                                "{}: {:?} {:#?} {:?}",
                                "Binary operator not implemented for ".red(),
                                op.left_type,
                                op,
                                op.right_type
                            ),
                        };
                        Some(Box::from(res))
                    }
                    (RawType::F32, RawType::F32)
                    if vec![
                        BinaryOperatorType::Addition,
                        BinaryOperatorType::Subtraction,
                        BinaryOperatorType::Multiplication,
                        BinaryOperatorType::Division,
                        BinaryOperatorType::Remainder,
                    ].contains(&op.operator_type) == false
                    => {
                        let left = self.build_basic_value(*left).unwrap();
                        let right = self.build_basic_value(*right).unwrap();
                        let res = match op.operator_type {
                            BinaryOperatorType::Addition => {
                                let res = self.builder.build_float_add(
                                    left.as_basic_value_enum().into_float_value(),
                                    right.as_basic_value_enum().into_float_value(),
                                    "add",
                                );
                                res.expect("build float add failed")
                            }
                            BinaryOperatorType::Subtraction => {
                                let res = self.builder.build_float_sub(
                                    left.as_basic_value_enum().into_float_value(),
                                    right.as_basic_value_enum().into_float_value(),
                                    "sub",
                                );
                                res.expect("build float sub failed")
                            }
                            BinaryOperatorType::Multiplication => {
                                let res = self.builder.build_float_mul(
                                    left.as_basic_value_enum().into_float_value(),
                                    right.as_basic_value_enum().into_float_value(),
                                    "mul",
                                );
                                res.expect("build float mul failed")
                            }
                            BinaryOperatorType::Division => {
                                let res = self.builder.build_float_div(
                                    left.as_basic_value_enum().into_float_value(),
                                    right.as_basic_value_enum().into_float_value(),
                                    "div",
                                );
                                res.expect("build float div failed")
                            }
                            BinaryOperatorType::Remainder => {
                                let res = self.builder.build_float_rem(
                                    left.as_basic_value_enum().into_float_value(),
                                    right.as_basic_value_enum().into_float_value(),
                                    "mod",
                                );
                                res.expect("build float mod failed")
                            }
                            _ => todo!(
                                "{}: {:?} {:#?} {:?}",
                                "Binary operator not implemented for ".red(),
                                op.left_type,
                                op,
                                op.right_type
                            ),
                        };
                        Some(Box::from(res))
                    }
                    (RawType::F32, RawType::F32) => {
                        let left = self.build_basic_value(*left).unwrap();
                        let right = self.build_basic_value(*right).unwrap();
                        let res = match op.operator_type {
                            BinaryOperatorType::Equals => {
                                let res = self.builder.build_float_compare(
                                    FloatPredicate::OEQ,
                                    left.as_basic_value_enum().into_float_value(),
                                    right.as_basic_value_enum().into_float_value(),
                                    "eq",
                                );
                                res.expect("build float compare failed")
                            }
                            BinaryOperatorType::NotEquals => {
                                let res = self.builder.build_float_compare(
                                    FloatPredicate::ONE,
                                    left.as_basic_value_enum().into_float_value(),
                                    right.as_basic_value_enum().into_float_value(),
                                    "ne",
                                );
                                res.expect("build float compare failed")
                            }
                            BinaryOperatorType::GreaterThan => {
                                let res = self.builder.build_float_compare(
                                    FloatPredicate::OGT,
                                    left.as_basic_value_enum().into_float_value(),
                                    right.as_basic_value_enum().into_float_value(),
                                    "gt",
                                );
                                res.expect("build float compare failed")
                            }
                            BinaryOperatorType::LessThan => {
                                let res = self.builder.build_float_compare(
                                    FloatPredicate::OLT,
                                    left.as_basic_value_enum().into_float_value(),
                                    right.as_basic_value_enum().into_float_value(),
                                    "lt",
                                );
                                res.expect("build float compare failed")
                            }
                            BinaryOperatorType::GreaterThanOrEqual => {
                                let res = self.builder.build_float_compare(
                                    FloatPredicate::OGE,
                                    left.as_basic_value_enum().into_float_value(),
                                    right.as_basic_value_enum().into_float_value(),
                                    "ge",
                                );
                                res.expect("build float compare failed")
                            }
                            BinaryOperatorType::LessThanOrEqual => {
                                let res = self.builder.build_float_compare(
                                    FloatPredicate::OLE,
                                    left.as_basic_value_enum().into_float_value(),
                                    right.as_basic_value_enum().into_float_value(),
                                    "le",
                                );
                                res.expect("build float compare failed")
                            }
                            _ => todo!(
                                "{}: {:?} {:#?} {:?}",
                                "Binary operator not implemented for ".red(),
                                op.left_type,
                                op,
                                op.right_type
                            ),
                        };
                        Some(Box::from(res))
                    }
                    _ => todo!(
                        "{}: {:?} {:#?} {:?}",
                        "Binary operator not implemented for ".red(),
                        op.left_type,
                        op,
                        op.right_type
                    ),
                }
            } // end of Binary
            CheckedExpression::Call {
                name,
                function,
                arguments,
            } => self.value_of_call(name, function, arguments),
            CheckedExpression::Conditional {
                condition,
                then,
                else_ifs,
                else_expr, _type,
            } => {
                self.build_conditional(condition, then, else_ifs, else_expr);
                None
            }
            CheckedExpression::Else { body } => self.build_block_may_contains_value(vec![*body]),
            _ => todo!(
                "{}: {:#?}",
                "cannot build basic value from Expression: \n".red(),
                expression
            ),
        }
    }

    fn value_of_call(
        &self,
        name: Token,
        function: FunctionType,
        arguments: Vec<CheckedExpression>,
    ) -> Option<Box<dyn BasicValue + '_>> {
        let mut args = Vec::new();
        for arg in arguments.iter() {
            args.push(
                BasicMetadataValueEnum::try_from(
                    self.build_basic_value(arg.clone())
                        .unwrap()
                        .as_any_value_enum(),
                )
                    .unwrap(),
            );
        }

        let res_name = format!("{}{}", "res_", &name.text);
        match self.module.get_function(&name.text) {
            Some(func) => {
                let res = self
                    .builder
                    .build_call(func, &args, &*res_name)
                    .expect(&*format!("Build Call of {} failed", &name.text));
                let res = res.try_as_basic_value().left().unwrap();
                Some(Box::from(res))
            }
            None => {
                self.diagnostics
                    .report(format!("Function {} not found", &name.text));
                None
            }
        }
    }

    fn build_conditional(
        &self,
        condition: Box<CheckedExpression>,
        then: Box<CheckedExpression>,
        else_ifs: Vec<CheckedExpression>,
        else_expr: Option<Box<CheckedExpression>>,
    ) {
        let entry_block = self.builder.get_insert_block().unwrap();

        let condition = self
            .build_basic_value(*condition)
            .unwrap()
            .as_basic_value_enum()
            .into_int_value();
        let then_block = self
            .context
            .append_basic_block(self.current_function.borrow().unwrap(), "then");
        let merge_block = self
            .context
            .append_basic_block(self.current_function.borrow().unwrap(), "merge");

        self.builder.position_at_end(then_block);
        self.build_ir(*then);
        self.builder
            .build_unconditional_branch(merge_block)
            .expect("build unconditional branch failed");

        if else_ifs.len() == 0 {
            if else_expr.is_some() {
                let else_block = self
                    .context
                    .append_basic_block(self.current_function.borrow().unwrap(), "else");
                self.builder.position_at_end(entry_block);
                self.builder
                    .build_conditional_branch(condition, then_block, else_block)
                    .expect("build conditional branch failed");

                self.builder.position_at_end(else_block);
                if let CheckedExpression::Else { body } = *else_expr.clone().unwrap() {
                    self.build_ir(*body);
                } else {
                    panic!(
                        "{}",
                        format!("{:#?} is not a else expression", else_expr).red()
                    )
                }
                self.builder
                    .build_unconditional_branch(merge_block)
                    .expect("build unconditional branch failed");
            } else {
                self.builder.position_at_end(entry_block);
                self.builder
                    .build_conditional_branch(condition, then_block, merge_block)
                    .expect("build conditional branch failed");
            }
        } else {
            let condition_blocks: Vec<_> = else_ifs
                .iter()
                .enumerate()
                .map(|(i, _)| {
                    self.context.append_basic_block(
                        self.current_function.borrow().unwrap(),
                        &format!("else_if_condition_{}", i),
                    )
                })
                .collect();

            let else_block = self
                .context
                .append_basic_block(self.current_function.borrow().unwrap(), "else");

            for (i, else_if) in else_ifs.into_iter().enumerate() {
                if let CheckedExpression::ElseIf { condition, body } = else_if {
                    self.builder.position_at_end(condition_blocks[i]);
                    let condition = self
                        .build_basic_value(*condition)
                        .unwrap()
                        .as_basic_value_enum()
                        .into_int_value();
                    let else_if_block = self.context.append_basic_block(
                        self.current_function.borrow().unwrap(),
                        &format!("else_if_{}", i),
                    );
                    let next_block = if i + 1 < condition_blocks.len() {
                        condition_blocks[i + 1]
                    } else {
                        else_block
                    };
                    self.builder
                        .build_conditional_branch(condition, else_if_block, next_block)
                        .expect("build conditional branch failed");

                    self.builder.position_at_end(else_if_block);
                    self.build_ir(*body);
                    self.builder
                        .build_unconditional_branch(merge_block)
                        .expect("build unconditional branch failed");
                } else {
                    panic!(
                        "{}",
                        format!("{:#?} is not a else if expression", else_if).red()
                    )
                }
            }

            self.builder.position_at_end(else_block);
            if let Some(else_expr) = else_expr {
                if let CheckedExpression::Else { body } = *else_expr {
                    self.build_ir(*body);
                } else {
                    panic!(
                        "{}",
                        format!("{:#?} is not a else expression", else_expr).red()
                    )
                }
            }
            self.builder
                .build_unconditional_branch(merge_block)
                .expect("build unconditional branch failed");

            self.builder.position_at_end(then_block);
            self.builder
                .build_unconditional_branch(merge_block)
                .expect("build unconditional branch failed");

            self.builder.position_at_end(entry_block);
            self.builder
                .build_conditional_branch(condition, then_block, condition_blocks[0])
                .expect("build conditional branch failed");
        }

        self.builder.position_at_end(merge_block);
    }

    fn build_valued_conditional(
        &self,
        condition: Box<CheckedExpression>,
        then: Box<CheckedExpression>,
        else_ifs: Vec<CheckedExpression>,
        else_expr: Option<Box<CheckedExpression>>,
    ) {
        let entry_block = self.builder.get_insert_block().unwrap();

        let condition = self
            .build_basic_value(*condition)
            .unwrap()
            .as_basic_value_enum()
            .into_int_value();
        let then_block = self
            .context
            .append_basic_block(self.current_function.borrow().unwrap(), "then");
        let merge_block = self
            .context
            .append_basic_block(self.current_function.borrow().unwrap(), "merge");

        self.builder.position_at_end(then_block);
        let then_val = self.build_block_may_contains_value(vec![*then.clone()]).unwrap();
        self.builder
            .build_unconditional_branch(merge_block)
            .expect("build unconditional branch failed");

        self.builder.position_at_end(merge_block);
        let current_func_ret_type = self.current_function.borrow().unwrap().get_type().get_return_type().unwrap();

        let phi = self.builder.build_phi(current_func_ret_type, "phi_tmp").expect("build phi failed");
        phi.add_incoming(&[(&then_val.as_basic_value_enum().clone(), then_block)]);

        if else_ifs.len() == 0 {
            if else_expr.is_some() {
                let else_block = self
                    .context
                    .append_basic_block(self.current_function.borrow().unwrap(), "else");
                self.builder.position_at_end(entry_block);
                self.builder
                    .build_conditional_branch(condition, then_block, else_block)
                    .expect("build conditional branch failed");

                self.builder.position_at_end(else_block);
                if let CheckedExpression::Else { body } = *else_expr.clone().unwrap() {
                    let else_val = self.build_block_may_contains_value(vec![*body]).unwrap();
                    phi.add_incoming(&[(&else_val.as_basic_value_enum().clone(), else_block)]);
                } else {
                    panic!(
                        "{}",
                        format!("{:#?} is not a else expression", else_expr).red()
                    )
                }
                self.builder
                    .build_unconditional_branch(merge_block)
                    .expect("build unconditional branch failed");
            } else {
                self.builder.position_at_end(entry_block);
                self.builder
                    .build_conditional_branch(condition, then_block, merge_block)
                    .expect("build conditional branch failed");
            }
        } else {
            let condition_blocks: Vec<_> = else_ifs
                .iter()
                .enumerate()
                .map(|(i, _)| {
                    self.context.append_basic_block(
                        self.current_function.borrow().unwrap(),
                        &format!("else_if_condition_{}", i),
                    )
                })
                .collect();

            let else_block = self
                .context
                .append_basic_block(self.current_function.borrow().unwrap(), "else");

            for (i, else_if) in else_ifs.into_iter().enumerate() {
                if let CheckedExpression::ElseIf { condition, body } = else_if {
                    self.builder.position_at_end(condition_blocks[i]);
                    let condition = self
                        .build_basic_value(*condition)
                        .unwrap()
                        .as_basic_value_enum()
                        .into_int_value();
                    let else_if_block = self.context.append_basic_block(
                        self.current_function.borrow().unwrap(),
                        &format!("else_if_{}", i),
                    );
                    let next_block = if i + 1 < condition_blocks.len() {
                        condition_blocks[i + 1]
                    } else {
                        else_block
                    };
                    self.builder
                        .build_conditional_branch(condition, else_if_block, next_block)
                        .expect("build conditional branch failed");

                    self.builder.position_at_end(else_if_block);
                    let else_if_val = self.build_block_may_contains_value(vec![*body]).unwrap();
                    phi.add_incoming(&[(&else_if_val.as_basic_value_enum().clone(), else_if_block)]);
                    self.builder
                        .build_unconditional_branch(merge_block)
                        .expect("build unconditional branch failed");
                } else {
                    panic!(
                        "{}",
                        format!("{:#?} is not a else if expression", else_if).red()
                    )
                }
            }

            self.builder.position_at_end(else_block);
            if let Some(else_expr) = else_expr {
                if let CheckedExpression::Else { body } = *else_expr {
                    let else_val = self.build_block_may_contains_value(vec![*body]).unwrap();
                    phi.add_incoming(&[(&else_val.as_basic_value_enum().clone(), else_block)]);
                } else {
                    panic!(
                        "{}",
                        format!("{:#?} is not a else expression", else_expr).red()
                    )
                }
            }
            self.builder
                .build_unconditional_branch(merge_block)
                .expect("build unconditional branch failed");

            self.builder.position_at_end(entry_block);
            self.builder
                .build_conditional_branch(condition, then_block, condition_blocks[0])
                .expect("build conditional branch failed");
        }

        self.builder.position_at_end(merge_block);
        self.builder.build_return(Some(&phi.as_basic_value())).expect("build return failed");
    }

    fn build_block(&self, expressions: Vec<CheckedExpression>) {
        for e in expressions {
            self.build_ir(e);
        }
    }

    fn build_block_may_contains_value(
        &self,
        expressions: Vec<CheckedExpression>,
    ) -> Option<Box<dyn BasicValue + '_>> {
        if expressions.len() < 2 {
            for e in &expressions[0..expressions.len() - 1] {
                self.build_ir(e.clone());
            }
        }
        if let Some(last) = expressions.last() {
            self.build_basic_value(last.clone())
        } else {
            None
        }
    }
}
