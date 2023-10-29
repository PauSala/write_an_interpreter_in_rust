use super::{
    program::Program,
    statements::{
        expression_statement::ExpressionStatement,
        expressions::{
            BlockStatement, Boolean, CallExpression, FunctionLiteral, Identifier, IfExpression,
            InfixExpression, IntegerLiteral, PrefixExpression,
        },
        let_statement::LetStatement,
        return_statement::ReturnStatement,
    }, ast::Statement,
};

#[derive(Debug)]
pub enum AstNodeType {
    Program(Program),
    ExpressionStatement(ExpressionStatement),
    IntegerLiteral(IntegerLiteral),
    Boolean(Boolean),
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    Identifier(Identifier),
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
    BlockStatement(BlockStatement),
    IfExpression(IfExpression),
    FunctionLiteral(FunctionLiteral),
    CallExpression(CallExpression),
    Statement(Box<dyn Statement>)
}

#[derive(Debug)]
pub struct AstNode {
    pub node: AstNodeType,
}
