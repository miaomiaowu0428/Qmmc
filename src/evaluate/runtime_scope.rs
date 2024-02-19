#![allow(dead_code)]

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use colored::Colorize;

use crate::analyze::diagnostic::DiagnosticBag;
use crate::evaluate::function::Function;
use crate::evaluate::Value;
use crate::evaluate::variable::Variable;

pub type VariableMap = HashMap<String, Variable>;

#[derive(Debug)]
pub struct RuntimeScope {
    pub parent: Option<Rc<RuntimeScope>>,
    pub values: RefCell<VariableMap>,
    pub(crate) diagnostics: DiagnosticBag,
}


impl RuntimeScope {
    pub fn new_global() -> Self {
        Self {
            parent: None,
            values: RefCell::new(HashMap::new()),
            diagnostics: DiagnosticBag::new(),
        }
    }
    pub fn with_parent(parent: Option<Rc<RuntimeScope>>) -> Self {
        Self {
            parent,
            values: RefCell::new(HashMap::new()),
            diagnostics: DiagnosticBag::new(),
        }
    }

    pub fn get_local(&self, name: &str) -> Option<Variable> {
        let values = self.values.borrow();
        values.get(name).cloned()
    }

    pub fn set_local(&self, name: &str, variable: Variable) {
        let mut values = self.values.borrow_mut();
        values.insert(name.to_string(), variable);
    }


    pub fn get_global(&self, name: &str) -> Option<Variable> {
        if let Some(v) = self.get_local(name) {
            Some(v)
        } else {
            if let Some(parent) = &self.parent {
                parent.get_global(name)
            } else {
                None
            }
        }
    }

    pub fn try_set_global(&self, name: &str, variable: Variable) {
        if let Some(v) = self.get_local(name) {
            self.set_local(name, variable);
        } else if let Some(parent) = &self.parent {
            parent.try_set_global(name, variable);
        }
    }

    pub fn declare_function(&self, name: &str, function: Function) {
        self.values.borrow_mut().insert(name.to_string(), Variable::init_as_immutable(
            Value::fun { fun: function.clone() },
            function.declared_expression.clone()
        ));
    }


    pub fn variables_to_string(&self) -> String {
        let mut result = String::new();
        let values = self.values.borrow();
        for (name, variable) in values.iter() {
            let chinese_count = name.chars().filter(|c| !c.is_ascii()).count();
            let name = if let Value::fun { .. } = variable.value {
                name.blue().to_string()
            } else if variable.mutable {
                name.purple().to_string()
            } else {
                name.green().to_string()
            };
            let width = 20 - chinese_count;
            result.push_str(&format!("{:<width$}: {}\n", name, variable.value));
        }
        result
    }
}


