use std::cell::RefCell;

use colored::Colorize;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::IntPredicate;
use inkwell::module::Module;
use inkwell::values::BasicMetadataValueEnum;
use inkwell::values::BasicValue;
use inkwell::values::BasicValueEnum;
use inkwell::values::FunctionValue;

use crate::analyze::diagnostic::DiagnosticBag;
use crate::analyze::lex::Token;
use crate::compile::{BinaryOperatorType, CheckedExpression, ConstExpr, FunctionDeclare, RawType, UnaryOperatorType};

pub struct IRBuilder<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: ExecutionEngine<'ctx>,
    current_function: RefCell<Option<FunctionValue<'ctx>>>,
    pub(crate) diagnostics: DiagnosticBag,
}

impl<'ctx> IRBuilder<'ctx> {
    pub fn new(
        context: &'ctx Context,
        module: Module<'ctx>,
        builder: Builder<'ctx>,
        execution_engine: ExecutionEngine<'ctx>
    ) -> IRBuilder<'ctx> {
        IRBuilder {
            context,
            module,
            builder,
            execution_engine,
            current_function: RefCell::new(None),
            diagnostics: DiagnosticBag::new(),
        }
    }

    pub fn save_as(&self, path: &str) {
        let path = std::path::Path::new(path);
        match self.module.print_to_file(&path) {
            Ok(_) => println!("{}",format!("{}: {}","Successfully wrote to file".green(), path.display()).green()),
            Err(e) => self.diagnostics.report(format!("Failed to write to file: {}", e.to_string())),
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
                    self.diagnostics.report(format!("{} at expr({}): can only declare {} or {} in global found {:#?}",
                                                    "Error".to_string().red(),
                                                    i,
                                                    "function".to_string().green(),
                                                    "variable".to_string().green(),
                                                    expr));
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
                self.builder.build_return(self.build_basic_value(*expression).as_deref()).expect("build return failed");
            }
            CheckedExpression::Block { expressions } => { self.build_block(expressions); }
            CheckedExpression::If { condition, then, r#else } => {
                self.build_if(self.current_function.borrow().unwrap(), CheckedExpression::If { condition, then, r#else });
            }

            _ => todo!("\n{}{:#?}", "cannot build instruction from Expression not implemented: \n".red(), expression)
        }

        // println!("Current Block: {:#?}", self.builder.get_insert_block());
    }

    fn build_function_declare(&self, name: Token, function: FunctionDeclare) {
        let LLVM_function_type = {
            let mut types = vec![];
            for param in function._type.param_types {
                match param {
                    RawType::I32 => types.push(self.context.i32_type().into()),
                    RawType::F32 => types.push(self.context.f32_type().into()),
                    RawType::Bool => types.push(self.context.bool_type().into()),
                    _ => todo!("{}", "cannot convert to function type".red())
                }
            }
            let fun_type = match *function._type.return_type {
                RawType::I32 => self.context.i32_type().fn_type(&types, false),
                RawType::F32 => self.context.f32_type().fn_type(&types, false),
                RawType::Bool => self.context.bool_type().fn_type(&types, false),
                RawType::None => self.context.void_type().fn_type(&types, false)
            };
            fun_type
        };

        let LLVM_function_OBJ = self.module.add_function(&name.text, LLVM_function_type, None);
        *self.current_function.borrow_mut() = Some(LLVM_function_OBJ);

        let basic_block = self.context.append_basic_block(LLVM_function_OBJ, "entry");
        self.builder.position_at_end(basic_block);
        self.build_func_body(function.body);
    }


    fn build_basic_value(&self, expression: CheckedExpression) -> Option<Box<dyn BasicValue + '_>> {
        match expression {
            CheckedExpression::Statement { expression } => {
                self.build_ir(*expression);
                None
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
                },
            },
            CheckedExpression::Unary { op, operand } => {
                match (op.operator_type, op.operand_type) {
                    (UnaryOperatorType::Positive, RawType::I32) | (UnaryOperatorType::Positive, RawType::F32) => {
                        self.build_basic_value(*operand).or(None)
                    }
                    (UnaryOperatorType::Negation, RawType::I32) => {
                        let v = self.build_basic_value(*operand).unwrap();
                        if let BasicValueEnum::IntValue(i) = v.as_basic_value_enum() {
                            Some(Box::from(self.builder.build_int_neg(i, "neg").expect("build i32 negation failed")))
                        } else {
                            self.diagnostics.report(format!("'-' is not defined for {:?}", v));
                            None
                        }
                    }
                    (UnaryOperatorType::Negation, RawType::F32) => {
                        let v = self.build_basic_value(*operand).unwrap();
                        if let BasicValueEnum::FloatValue(f) = v.as_basic_value_enum() {
                            Some(Box::from(self.builder.build_float_neg(f, "neg").expect("build f32 negation failed")))
                        } else {
                            self.diagnostics.report(format!("'-' is not defined for {:?}", v));
                            None
                        }
                    }
                    (UnaryOperatorType::LogicalNegation, RawType::Bool) => {
                        let v = self.build_basic_value(*operand).unwrap();
                        if let BasicValueEnum::IntValue(i) = v.as_basic_value_enum() {
                            Some(Box::from(self.builder.build_not(i, "not").expect("build logical negation failed")))
                        } else {
                            self.diagnostics.report("'not' is not defined for non-integer types".to_string());
                            None
                        }
                    }
                    _ => todo!("{}{:#?}", "Unary operator not implemented for ".red(), operand)
                }
            }// end of Unary
            CheckedExpression::Binary { op, left, right } => {
                match (op.left_type, op.operator_type, op.right_type) {
                    (RawType::I32, BinaryOperatorType::Addition, RawType::I32) => {
                        let l = self.build_basic_value(*left).unwrap().as_basic_value_enum().into_int_value();
                        let r = self.build_basic_value(*right).unwrap().as_basic_value_enum().into_int_value();
                        Some(Box::from(self.builder.build_int_add(l, r, "add").expect("build i32 addition failed")))
                    }
                    (RawType::I32, BinaryOperatorType::Equals, RawType::I32) => {
                        let l = self.build_basic_value(*left).unwrap().as_basic_value_enum().into_int_value();
                        let r = self.build_basic_value(*right).unwrap().as_basic_value_enum().into_int_value();
                        Some(Box::from(self.builder.build_int_compare(IntPredicate::EQ, l, r, "sub").expect("build i32 subtraction failed")))
                    }
                    (RawType::Bool, BinaryOperatorType::Equals, RawType::Bool) => {
                        let l = self.build_basic_value(*left).unwrap().as_basic_value_enum().into_int_value();
                        let r = self.build_basic_value(*right).unwrap().as_basic_value_enum().into_int_value();
                        Some(Box::from(self.builder.build_int_compare(IntPredicate::EQ, l, r, "sub").expect("build i32 subtraction failed")))
                    }
                    (RawType::Bool, BinaryOperatorType::NotEquals, RawType::Bool) => {
                        let l = self.build_basic_value(*left).unwrap().as_basic_value_enum().into_int_value();
                        let r = self.build_basic_value(*right).unwrap().as_basic_value_enum().into_int_value();
                        Some(Box::from(self.builder.build_int_compare(IntPredicate::NE, l, r, "sub").expect("build i32 subtraction failed")))
                    }
                    (RawType::I32, BinaryOperatorType::GreaterThan, RawType::I32) => {
                        let l = self.build_basic_value(*left).unwrap().as_basic_value_enum().into_int_value();
                        let r = self.build_basic_value(*right).unwrap().as_basic_value_enum().into_int_value();
                        Some(Box::from(self.builder.build_int_compare(IntPredicate::SGT, l, r, "sub").expect("build i32 subtraction failed")))
                    }
                    _ => todo!("{}: {:?} {:#?} {:?}", "Binary operator not implemented for ".red(), op.left_type, op, op.right_type)
                }
            }// end of Binary
            CheckedExpression::Call { name, function, arguments } => {
                let mut args = Vec::new();
                for arg in arguments.iter() {
                    args.push(BasicMetadataValueEnum::try_from(self.build_basic_value(arg.clone()).unwrap().as_any_value_enum()).unwrap());
                }

                let res_name = format!("{}{}", "res_", &name.text);
                match self.module.get_function(&name.text) {
                    Some(func) => {
                        let res = self.builder.build_call(func, &args, &*res_name).expect(&*format!("Build Call of {} failed", &name.text));
                        let res = res.try_as_basic_value().left().unwrap();
                        Some(Box::from(res))
                    },
                    None => {
                        self.diagnostics.report(format!("Function {} not found", &name.text));
                        None
                    }
                }
            }
            CheckedExpression::If { condition, then, r#else } => {
                self.build_if(self.current_function.borrow().unwrap(), CheckedExpression::If { condition, then, r#else });
                None
            }
            _ => todo!("{}: {:#?}", "cannot build basic value from Expression: \n".red(), expression)
        }
    }

    fn build_if(&self, function_value: FunctionValue, checked_expression: CheckedExpression) {
        if let CheckedExpression::If { condition, then, r#else } = checked_expression {
            let condition = self.build_basic_value(*condition).unwrap().as_basic_value_enum().into_int_value();
            let then_block = self.context.append_basic_block(function_value, "then");
            let else_block = self.context.append_basic_block(function_value, "else");
            let merge_block = self.context.append_basic_block(function_value, "merge");

            self.builder.build_conditional_branch(condition, then_block, else_block).expect("build conditional branch failed");

            self.builder.position_at_end(then_block);
            self.build_ir(*then);
            self.builder.build_unconditional_branch(merge_block).expect("build unconditional branch failed");

            self.builder.position_at_end(else_block);
            if let Some(else_expr) = r#else {
                self.build_ir(*else_expr);
            }
            self.builder.build_unconditional_branch(merge_block).expect("build unconditional branch failed");


            self.builder.position_at_end(merge_block);
        }
    }

    fn build_func_body(&self, body: CheckedExpression) {
        if let CheckedExpression::Block { expressions } = body {
            for e in expressions.iter().take(expressions.len() - 1) {
                match e {
                    CheckedExpression::If { condition, then, r#else } => {
                        self.build_if(self.current_function.borrow().unwrap(), e.clone());
                    }
                    _ => { self.build_ir(e.clone()); }
                }
            }
            if let Some(last) = expressions.last() {
                match last {
                    CheckedExpression::Return { expression } => {
                        self.builder.build_return(self.build_basic_value(*expression.clone()).as_deref()).unwrap();
                    }
                    CheckedExpression::If { condition, then, r#else } => {
                        self.build_if(self.current_function.borrow().unwrap(), last.clone());
                    }
                    _ => { self.builder.build_return(self.build_basic_value(last.clone()).as_deref()).unwrap(); }
                }
            }
        } else { self.diagnostics.report("Function body must be a block".to_string()); }
    }
    fn build_block(&self, expressions: Vec<CheckedExpression>) {
        for e in expressions {
            self.build_ir(e);
        }
    }
}



