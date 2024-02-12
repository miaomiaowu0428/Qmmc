#![allow(dead_code)]

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use crate::evaluate::value::Value;
use crate::evaluate::variable::Variable;

pub type ValueMap = HashMap<String, Variable>;

pub struct RuntimeScope {
    pub parent: Option<Rc<RuntimeScope>>,
    pub values: RefCell<ValueMap>
}

impl RuntimeScope {
    pub fn new(parent: Option<Rc<RuntimeScope>>) -> Self {
        Self {
            parent,
            values: RefCell::new(HashMap::new())
        }
    }

    pub fn get(&self, name: String) -> Option<Value> {
        match self.values.borrow().get(name.as_str()) {
            Some(variable) => Some(variable.value.clone()),
            None => match &self.parent {
                Some(parent) => parent.get(name),
                None => None
            }
        }
    }

    pub fn set(&self, name:String, variable: Variable) {
        self.values.borrow_mut().insert(name, variable);
    }
}


