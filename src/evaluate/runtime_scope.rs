#![allow(dead_code)]

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Debug;
use std::rc::Rc;

use colored::Colorize;

use crate::analyze::diagnostic::DiagnosticBag;
use crate::evaluate::variable::Variable;

pub type ValueMap = HashMap<String, Variable>;

pub struct RuntimeScope {
    pub parent: Option<Rc<RuntimeScope>>,
    pub values: RefCell<ValueMap>,
    pub(crate) diagnostics: DiagnosticBag,
}

impl Debug for RuntimeScope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let values = self.values.borrow();
        let data = values.iter().map(|(name, variable)| {
            let chinese_count = name.chars().filter(|c| !c.is_ascii()).count();
            let width = 8 - chinese_count;
            let name = if variable.mutable { name.underline() } else { name.normal() };
            format!("{:^width$}: {:?}, \n", name, variable.value, width = width)
        }).collect::<String>();

        write!(f, "{}", data)
    }
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

    pub fn set_global(&self, name: &str, variable: Variable) {
        if let Some(v) = self.get_local(name) {
            self.set_local(name, variable);
        } else if let Some(parent) = &self.parent {
            parent.set_global(name, variable);
        }
    }
}


