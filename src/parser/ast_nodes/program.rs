use super::{statements::Statement, Node};

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new() -> Self {
        Program {
            statements: Vec::new(),
        }
    }
}

impl Node for Program {
    fn token_literal(&self) -> String {
        if !self.statements.is_empty() {
            self.statements[0].token_literal()
        } else {
            "".to_string()
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
