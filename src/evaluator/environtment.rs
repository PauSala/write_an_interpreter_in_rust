use std::collections::HashMap;

use super::types::Object;

#[derive(Debug, Clone)]
pub struct Environtment {
    pub store: HashMap<String, Object>
}

impl Environtment {
    pub fn new() -> Environtment{
        Environtment { store: HashMap::new() }
    }

    pub fn get(&mut self, key: &str) -> Option<&Object>{
        self.store.get(key)
    }

    pub fn set(&mut self, key: &str, obj: Object) {
        self.store.insert(key.to_string(), obj);
    }
}
