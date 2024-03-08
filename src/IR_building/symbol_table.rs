use std::cell::RefCell;
use std::collections::HashMap;

use inkwell::values::PointerValue;

pub struct SymbolTable<'ctx> {
    stack: RefCell<Vec<HashMap<String, PointerValue<'ctx>>>>,
}

impl<'ctx> SymbolTable<'ctx> {
    pub fn new() -> Self {
        Self {
            stack: RefCell::new(vec![HashMap::new()]),
        }
    }

    pub fn push_scope(&self) {
        self.stack.borrow_mut().push(HashMap::new());
    }

    pub fn pop_scope(&self) {
        self.stack.borrow_mut().pop();
    }

    pub fn insert(&self, name: String, value: PointerValue<'ctx>) {
        if let Some(scope) = self.stack.borrow_mut().last_mut() {
            scope.insert(name, value);
        }
    }

    pub fn get(&self, name: &str) -> Option<PointerValue<'ctx>> {
        for scope in self.stack.borrow().iter().rev() {
            if let Some(value) = scope.get(name) {
                return Some(*value);
            }
        }
        None
    }
}

pub struct ScopeGuard<'a, 'ctx> {
    symbol_table: &'a SymbolTable<'ctx>,
}

impl<'a, 'ctx> ScopeGuard<'a, 'ctx> {
    pub fn new(symbol_table: &'a SymbolTable<'ctx>) -> Self {
        symbol_table.push_scope();
        Self { symbol_table }
    }
}

impl<'a, 'ctx> Drop for ScopeGuard<'a, 'ctx> {
    fn drop(&mut self) {
        self.symbol_table.pop_scope();
    }
}
