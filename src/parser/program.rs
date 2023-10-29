use super::ast::{Node, Statement};

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
}
impl Program {
    pub fn new() -> Self {
        Program {
            statements: Vec::new(),
        }
    }
}
impl Node for Program {
    fn token_literal(&self) -> Option<String> {
        if !self.statements.is_empty() {
            self.statements[0].token_literal()
        } else {
            None
        }
    }
    fn string(&self) -> String {
        let mut buffer = String::new();
        for item in &self.statements {
            let value = item.string();
            buffer.push_str(&value)
        }
        buffer
    }
}
