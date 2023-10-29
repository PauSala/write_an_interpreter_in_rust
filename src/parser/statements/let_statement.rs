use std::{any::Any, rc::Rc};

use crate::{
    parser::ast::{Expression, Node, Statement, NodeType},
    token::Token,
};

use super::expressions::Identifier;

#[derive(Debug)]
pub struct LetStatement {
    pub(crate) token: Token,
    pub name: Identifier,
    pub value: Option<Rc<dyn Expression>>,
}

impl Node for LetStatement {
    fn token_literal(&self) -> Option<String> {
        Some(self.token.literal.clone())
    }
    fn string(&self) -> String {
        let mut buffer = String::new();
        let value = self.token_literal().unwrap();
        let name = self.name.string();
        buffer.push_str(&format!("{} ", value.clone()));
        buffer.push_str(&name);
        buffer.push_str("=");
        if let Some(value) = &self.value {
            buffer.push_str(&value.string());
        }
        buffer.push_str(";");
        buffer
    }

    fn node_type(&self) -> NodeType {
        NodeType::LetStatement
    }
}

impl Statement for LetStatement {
    fn statement_node(&self) -> Option<String> {
        None
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}
