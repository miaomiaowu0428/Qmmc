use std::cell::RefCell;

use crate::analyze::diagnostic::DiagnosticBag;
use crate::analyze::lex::Token;
use crate::compile::{
    BinaryOperatorType, CheckedExpression, ConstExpr, FunctionDeclare, FunctionType, RawType,
    UnaryOperatorType,
};
use crate::IR_building::loop_info::{LoopGuard, LoopStack};
use crate::IR_building::symbol_table::{ScopeGuard, SymbolTable};
use colored::Colorize;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{AnyType, AnyTypeEnum, BasicType, BasicTypeEnum, StructType};
use inkwell::values::BasicValue;
use inkwell::values::BasicValueEnum;
use inkwell::values::FunctionValue;
use inkwell::values::{AnyValue, BasicMetadataValueEnum, PointerValue};
use inkwell::{FloatPredicate, IntPredicate};

pub struct IRBuilder<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    current_function: RefCell<Option<FunctionValue<'ctx>>>,
    symbol_table: SymbolTable<'ctx>,
    loop_stack: LoopStack<'ctx>,
    zst: BasicTypeEnum<'ctx>,
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
            zst: BasicTypeEnum::from(context.struct_type(&[], false)),
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
                CheckedExpression::VarDeclare {
                    name,
                    _type,
                    init_expr,
                } => {
                    self.build_global_variable(&*name.text, _type, init_expr);
                }
                CheckedExpression::FunctionDeclaration { name, function } => {
                    self.build_function_declare(name.clone(), *function.clone());
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

    fn build_global_variable(
        &self,
        name: &str,
        _type: &RawType,
        init_expr: &Box<CheckedExpression>,
    ) {
        let LLVM_Type = match _type {
            RawType::I32 => self.context.i32_type().as_any_type_enum(),
            RawType::F32 => self.context.f32_type().as_any_type_enum(),
            RawType::Bool => self.context.bool_type().as_any_type_enum(),
            _ => todo!(
                "{}",
                format!("{}{:#?}", "cannot convert to global variable type", _type).red()
            ),
        };
        let var = self.module.add_global(
            BasicTypeEnum::try_from(LLVM_Type)
                .expect(&format!("{} is not a basic type", LLVM_Type)),
            None,
            name,
        );
        var.set_initializer(&var);
        self.symbol_table
            .insert(name.to_string(), var.as_pointer_value());
    }

    fn build_loop(&self, body: CheckedExpression) {
        if let CheckedExpression::Block { expressions } = body {
            let scope_guard = ScopeGuard::new(&self.symbol_table);

            let loop_block = self
                .context
                .append_basic_block(self.current_function.borrow().unwrap(), "loop");
            let merge_block = self
                .context
                .append_basic_block(self.current_function.borrow().unwrap(), "merge");

            let loop_guard = LoopGuard::new(&self.loop_stack, merge_block, loop_block);

            self.builder
                .build_unconditional_branch(loop_block)
                .expect("build unconditional branch failed");

            self.builder.position_at_end(loop_block);
            for e in expressions {
                let _ = self.build_any_value(e);
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
        if let CheckedExpression::Block { expressions } = *body {
            for e in expressions {
                let _ = self.build_any_value(e);
            }
        }
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
            for (i, e) in expressions.iter().enumerate() {
                let value = self.build_any_value(e.clone());
                if i == expressions.len() - 1 {
                    self.builder
                        .build_return(Some(
                            &BasicValueEnum::try_from(value.as_any_value_enum()).expect(
                                format!("{:?} is not a basic value", value.as_any_value_enum())
                                    .as_str(),
                            ),
                        ))
                        .expect("build return failed");
                }
            }
        } else {
            self.diagnostics
                .report("Function body must be a block".to_string());
        }
    }

    fn build_basic_value(&self, expression: CheckedExpression) -> Option<Box<dyn BasicValue + '_>> {
        let value = self.build_any_value(expression);
        if let Ok(value) = BasicValueEnum::try_from(value.as_any_value_enum()) {
            Some(Box::from(value))
        } else {
            self.diagnostics
                .report(format!("{:?} is not a basic value", value));
            None
        }
    }
    fn build_any_value(&self, expression: CheckedExpression) -> Box<dyn AnyValue + '_> {
        // println!("{}: {:#?}", "building any value".green(), expression);

        match expression {
            CheckedExpression::Statement { expression } => {
                self.build_any_value(*expression);
                self.new_zst_value()
            }
            CheckedExpression::Break => {
                let merge_block = self.loop_stack.top().unwrap().merge_block;
                self.builder
                    .build_unconditional_branch(merge_block)
                    .expect("build unconditional branch failed");
                self.new_zst_value()
            }
            CheckedExpression::Return { expression } => {
                let ret_block = self.context.append_basic_block(self.current_function.borrow().unwrap(), "return");
                let merge_block = self.context.append_basic_block(self.current_function.borrow().unwrap(), "merge");

                self.builder.build_conditional_branch(
                    self.context.bool_type().const_int(1, false),
                    ret_block,
                    merge_block,
                ).expect("build conditional branch failed");

                self.builder.position_at_end(ret_block);
                self.builder
                    .build_return(self.build_basic_value(*expression).as_deref())
                    .expect("build return failed");

                self.builder.position_at_end(merge_block);

                self.new_zst_value()
            }
            CheckedExpression::Block { expressions } => self.build_valued_block(expressions),
            CheckedExpression::VarDeclare { name, _type, init_expr }=>self.build_var_declare(name, _type, init_expr),
            CheckedExpression::Identifier { name } => {
                println!("{}", format!("scope: {:#?}", self.symbol_table).green());
                let var = self.symbol_table.get(&name.text).expect(&format!("{} not found", name.text.red()));
                Box::from(
                    self.builder
                        .build_load(PointerValue::try_from(var).unwrap(), &name.text)
                        .expect("build load failed"),
                )
            }
            CheckedExpression::Literal { value } => match value {
                ConstExpr::I32(i) => {
                    let i32_type = self.context.i32_type();
                    let i32_value = i32_type.const_int(i as u64, false);
                    Box::from(i32_value)
                }
                ConstExpr::Bool(b) => {
                    let bool_type = self.context.bool_type();
                    let bool_value = bool_type.const_int(b as u64, false);
                    Box::from(bool_value)
                }
                ConstExpr::F32(f) => {
                    let f32_type = self.context.f32_type();
                    let f32_value = f32_type.const_float(f as f64);
                    Box::from(f32_value)
                }
                ConstExpr::None => {
                    self.diagnostics.report("Invalid literal: None".to_string());
                    self.new_zst_value()
                }
            },
            CheckedExpression::Unary { op, operand } => match (op.operator_type, op.operand_type) {
                (UnaryOperatorType::Positive, RawType::I32)
                | (UnaryOperatorType::Positive, RawType::F32) => self.build_any_value(*operand),
                (UnaryOperatorType::Negation, RawType::I32) => {
                    let v = self.build_basic_value(*operand).unwrap();
                    if let BasicValueEnum::IntValue(i) = v.as_basic_value_enum() {
                        Box::from(
                            self.builder
                                .build_int_neg(i, "neg")
                                .expect("build i32 negation failed"),
                        )
                    } else {
                        self.diagnostics
                            .report(format!("'-' is not defined for {:?}", v));
                        self.new_zst_value()
                    }
                }
                (UnaryOperatorType::Negation, RawType::F32) => {
                    let v = self.build_basic_value(*operand).unwrap();
                    if let BasicValueEnum::FloatValue(f) = v.as_basic_value_enum() {
                        Box::from(
                            self.builder
                                .build_float_neg(f, "neg")
                                .expect("build f32 negation failed"),
                        )
                    } else {
                        self.diagnostics
                            .report(format!("'-' is not defined for {:?}", v));
                        self.new_zst_value()
                    }
                }
                (UnaryOperatorType::LogicalNegation, RawType::Bool) => {
                    let v = self.build_basic_value(*operand).unwrap();
                    if let BasicValueEnum::IntValue(i) = v.as_basic_value_enum() {
                        Box::from(
                            self.builder
                                .build_not(i, "not")
                                .expect("build logical negation failed"),
                        )
                    } else {
                        self.diagnostics
                            .report("'not' is not defined for non-integer types".to_string());
                        self.new_zst_value()
                    }
                }

                _ => todo!(
                    "{}{:#?}",
                    "Unary operator not implemented for ".red(),
                    operand
                ),
            }, // end of Unary
            CheckedExpression::Binary { op, left, right } => match (op.left_type, op.right_type) {
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
                    Box::from(res)
                }
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
                    Box::from(res)
                }
                (RawType::F32, RawType::F32)
                if vec![
                    BinaryOperatorType::Addition,
                    BinaryOperatorType::Subtraction,
                    BinaryOperatorType::Multiplication,
                    BinaryOperatorType::Division,
                    BinaryOperatorType::Remainder,
                ]
                    .contains(&op.operator_type)
                    == false =>
                    {
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
                        Box::from(res)
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
                    Box::from(res)
                }
                _ => todo!(
                    "{}: {:?} {:#?} {:?}",
                    "Binary operator not implemented for ".red(),
                    op.left_type,
                    op,
                    op.right_type
                ),
            }, // end of Binary
            CheckedExpression::Call {
                name,
                function,
                arguments,
            } => self.any_value_of_call(name, function, arguments),
            CheckedExpression::Conditional {
                condition,
                then,
                else_ifs,
                else_expr,
                _type,
            } => self.build_conditional_val(condition, then, else_ifs, else_expr, _type),
            CheckedExpression::Else { body } => self.build_valued_block(vec![*body]),
            _ => todo!(
                "{}: {:#?}",
                "cannot build basic value from Expression: \n".red(),
                expression
            ),
        }
    }

    fn build_var_declare(
        &self,
        name: Token,
        _type: RawType,
        init_expr: Box<CheckedExpression>,
    ) -> Box<dyn AnyValue + '_> {
        let alloca = match _type {
            RawType::I32 => self.builder.build_alloca(self.context.i32_type(), &name.text),
            RawType::F32 => self.builder.build_alloca(self.context.f32_type(), &name.text),
            RawType::Bool => self.builder.build_alloca(self.context.bool_type(), &name.text),
            _ => todo!(
                "{}",
                format!("{}{:#?}", "cannot convert to alloca type", _type).red()
            ),
        }
        .unwrap();

        self.symbol_table.insert(name.text.clone(), alloca);
        println!("{} declared: {:#?}", name.text.green(), alloca);

        let init = self.build_basic_value(*init_expr).unwrap();
        self.builder
            .build_store(alloca, init.as_basic_value_enum())
            .expect("build store failed");
        self.new_zst_value()
    }

    fn any_value_of_call(
        &self,
        name: Token,
        function: FunctionType,
        arguments: Vec<CheckedExpression>,
    ) -> Box<dyn AnyValue + '_> {
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
                Box::from(res)
            }
            None => {
                self.diagnostics
                    .report(format!("Function {} not found", &name.text));
                self.new_zst_value()
            }
        }
    }


    fn llvm_type_from(&self, _type: RawType) -> BasicTypeEnum<'ctx> {
        match _type {
            RawType::Unit => {
                BasicTypeEnum::from(self.zst)
            }
            RawType::I32 => {
                self.context.i32_type().as_basic_type_enum()
            }
            RawType::Bool => {
                self.context.bool_type().as_basic_type_enum()
            }
            RawType::F32 => {
                self.context.f32_type().as_basic_type_enum()
            }
        }
    }

    fn current_block(&self) -> inkwell::basic_block::BasicBlock<'ctx> {
        self.builder.get_insert_block().unwrap()
    }

    fn build_conditional_val(
        &self,
        condition: Box<CheckedExpression>,
        then: Box<CheckedExpression>,
        else_ifs: Vec<CheckedExpression>,
        else_expr: Option<Box<CheckedExpression>>,
        _type: RawType,
    ) -> Box<dyn AnyValue + '_> {
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
        self.builder.position_at_end(merge_block);
        let phi = self.builder.build_phi(self.llvm_type_from(_type), "phi_tmp").unwrap();

        self.builder.position_at_end(then_block);

        let val = self.build_valued_block(then.unwrap());
        let val = BasicValueEnum::try_from(val.as_any_value_enum()).unwrap();
        phi.add_incoming(&[(&val, self.current_block())]);

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
                    let val = self.build_valued_block(body.unwrap());
                    let val = BasicValueEnum::try_from(val.as_any_value_enum()).unwrap();
                    phi.add_incoming(&[(&val, self.current_block())]);
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
                    let val = self.build_valued_block(body.unwrap());
                    let val = BasicValueEnum::try_from(val.as_any_value_enum()).unwrap();
                    phi.add_incoming(&[(&val, self.current_block())]);

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
                    let val = self.build_valued_block(body.unwrap());
                    let val = BasicValueEnum::try_from(val.as_any_value_enum()).unwrap();
                    phi.add_incoming(&[(&val, self.current_block())]);
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
        Box::from(phi)
    }

    fn build_valued_block(
        &self,
        expressions: Vec<CheckedExpression>,
    ) -> Box<dyn AnyValue + '_> {
        let scope_guard = ScopeGuard::new(&self.symbol_table);

        let mut res_val = self.new_zst_value();
        for e in expressions {
            res_val = self.build_any_value(e);
        }
        res_val
    }

    fn new_zst_value(&self) -> Box<dyn AnyValue + '_> {
        let zsv = self.zst.into_struct_type().const_zero();
        Box::from(zsv)
    }
}
