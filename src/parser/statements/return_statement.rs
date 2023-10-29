use crate::{
    parser::ast::{Expression, Node, Statement, NodeType},
    token::Token,
};
use std::{any::Any, rc::Rc};

#[derive(Debug)]
pub struct ReturnStatement {
    pub(crate) token: Token,
    pub return_value: Option<Rc<dyn Expression>>,
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> Option<String> {
        Some(self.token.literal.clone())
    }
    fn string(&self) -> String {
        let mut buffer = String::new();
        let value = self.token_literal().unwrap();
        buffer.push_str(&format!("{} ", value.clone()));
        if let Some(value) = &self.return_value {
            buffer.push_str(&value.string());
        }
        buffer.push_str(";");
        buffer
    }

    fn node_type(&self) -> NodeType {
        NodeType::ReturnStatement
    }
}

impl Statement for ReturnStatement {
    fn statement_node(&self) -> Option<String> {
        None
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}
