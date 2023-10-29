use std::{any::Any, rc::Rc};

use crate::{
    parser::ast::{Expression, Node, Statement, NodeType},
    token::Token,
};

#[derive(Debug)]
pub struct ExpressionStatement {
    pub(crate) token: Token,
    pub expression: Option<Rc<dyn Expression>>,
}

impl Statement for ExpressionStatement {
    fn statement_node(&self) -> Option<String> {
        None
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> Option<String> {
        Some(self.token.literal.clone())
    }
    fn string(&self) -> String {
        match &self.expression {
            None => "".to_string(),
            Some(value) => value.string(),
        }
    }
    fn node_type(&self) -> NodeType {
        NodeType::ExpressionStatement
    }
}
