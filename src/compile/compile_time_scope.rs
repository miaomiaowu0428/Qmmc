use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::compile::r#type::FunctionDeclare;
use crate::compile::variable_symbol::VariableSymbol;

type VariableMap = HashMap<String, VariableSymbol>;
type FunctionMap = HashMap<String, FunctionDeclare>;

#[derive(Debug)]
pub struct CompileTimeScope {
    pub parent: Option<Rc<CompileTimeScope>>,
    pub variables: RefCell<VariableMap>,
    pub functions: RefCell<FunctionMap>,
}


impl CompileTimeScope {
    pub fn new_global() -> Self {
        Self {
            parent: None,
            variables: RefCell::new(HashMap::new()),
            functions: RefCell::new(HashMap::new()),
        }
    }
    pub fn with_parent(parent: Option<Rc<CompileTimeScope>>) -> Self {
        Self {
            parent,
            variables: RefCell::new(HashMap::new()),
            functions: RefCell::new(HashMap::new()),
        }
    }

    pub fn get_local(&self, name: &str) -> Option<VariableSymbol> {
        let values = self.variables.borrow();
        values.get(name).cloned()
    }

    pub fn set_local(&self, name: &str, variable: VariableSymbol) {
        let mut values = self.variables.borrow_mut();
        values.insert(name.to_string(), variable);
    }

    pub fn get_global(&self, name: &str) -> Option<VariableSymbol> {
        if let Some(v) = self.get_local(name) {
            Some(v)
        } else if let Some(parent) = &self.parent {
            parent.get_global(name)
        } else {
            None
        }
    }


    pub fn declare_variable(&self, name: &str, variable: VariableSymbol) {
        let mut values = self.variables.borrow_mut();
        if values.contains_key(name) {
            // self.diagnostics.report_variable_already_declared(name);
        } else {
            values.insert(name.to_string(), variable);
        }
    }

    pub fn declare_function(&self, name: &str, function: FunctionDeclare) {
        let mut functions = self.functions.borrow_mut();
        if functions.contains_key(name) {
            println!("{}",
                     format!("Function {} already declared", name).as_str()
            )
        } else {
            functions.insert(name.to_string(), function);
        }
    }
    pub fn get_global_function(&self, name: &str) -> Option<FunctionDeclare> {
        if let Some(fun) = self.get_local_function(name) {
            Some(fun)
        } else if let Some(parent) = &self.parent {
            parent.get_global_function(name)
        } else {
            None
        }
    }

    pub fn get_local_function(&self, name: &str) -> Option<FunctionDeclare> {
        let functions = self.functions.borrow();
        functions.get(name).cloned()
    }


    pub fn try_set_global(&self, name: &str, variable: VariableSymbol) {
        if let Some(v) = self.get_local(name) {
            self.set_local(name, variable);
        } else if let Some(parent) = &self.parent {
            parent.try_set_global(name, variable);
        }
    }
}













