use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::types::Object;

#[derive(Debug, Clone)]
pub struct Environtment {
    pub store: HashMap<String, Object>,
    pub outer: Option<Rc<RefCell<Environtment>>>,
}

impl Environtment {
    pub fn new() -> Environtment {
        Environtment {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn get(&self, key: &str) -> Option<Object> {
        if let Some(value) = self.store.get(key) {
            return Some(value.clone());
        }
        match &self.outer {
            None => None,
            Some(env) => {
                let value = env.borrow_mut().get(key);
                value
            }
        }
    }

    pub fn set(&mut self, key: &str, obj: Object) {
        self.store.insert(key.to_string(), obj);
    }
}

pub fn enclosed_environtment(env: Rc<RefCell<Environtment>>) -> Environtment {
    let mut new_env = Environtment::new();
    new_env.outer = Some(env);
    new_env
}
