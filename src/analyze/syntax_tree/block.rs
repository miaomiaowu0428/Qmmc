use std::cell::RefCell;
use crate::analyze::syntax_tree::Expression;

pub struct Block {
    pub(crate) expressions: RefCell<Vec<Expression>>,
}


impl Block {
    pub fn new() -> Self {
        Self {
            expressions: RefCell::new(Vec::new()),
        }
    }

    pub fn add_expression(&self, expression: Expression) {
        self.expressions.borrow_mut().push(expression);
    }
    pub fn print(&self, indent: i32) {
        for expr in self.expressions.borrow().iter() {
            expr.print(indent);
        }
    }


}