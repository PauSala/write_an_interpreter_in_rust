use std::rc::Rc;

use crate::token::TokenType;

use super::{
    precedence::Precedence,
    Parser, ast_nodes::expressions::{Expression, Identifier, IntegerLiteral, PrefixExpression, InfixExpression, IfExpression, FunctionLiteral, BlockStatement, CallExpression, Boolean},
};

pub fn parse_identifier(parser: &mut Parser) -> Option<Rc<Expression>> {
    if let Some(current_token) = &parser.current_token {
        let token = current_token.clone();
        return Some(Rc::new(Expression::Identifier(Identifier {
            token: token.clone(),
            value: token.literal,
        })));
    }
    None
}

pub fn parse_integer_literal(parser: &mut Parser) -> Option<Rc<Expression>> {
    if let Some(token) = &parser.current_token {
        let current_token = token.clone();
        let value: Result<u64, _> = current_token.literal.parse();
        match value {
            Ok(integer) => {
                return Some(Rc::new(Expression::IntegerLiteral(IntegerLiteral {
                    token: current_token,
                    value: integer,
                })));
            }
            Err(e) => {
                let error_str = String::from(format!("Expected an integer, got: \n{:?}", e));
                parser.errors.push(error_str.to_string());
                return None;
            }
        }
    }
    None
}

pub fn parse_prefix_expression(parser: &mut Parser) -> Option<Rc<Expression>> {
    if let Some(token) = &parser.current_token {
        let current_token = token.clone();
        let mut expression = PrefixExpression {
            token: token.clone(),
            operator: current_token.literal,
            right: None,
        };
        parser.next_token();
        let right = parser.parse_expression(Precedence::PREFIX);
        expression.right = right;
        return Some(Rc::new(Expression::PrefixExpression(expression)));
    }
    None
}

pub fn parse_infix_expression(
    parser: &mut Parser,
    left: Rc<Expression>,
) -> Option<Rc<Expression>> {
    //WARNING ->> This token advance was originally in parse_expression,
    //it was moved moved due to borrowing parser problems in the caller (parse_expression)
    parser.next_token();
    if let Some(token) = &parser.current_token {
        let current_token = token.clone();
        let mut expression = InfixExpression {
            token: token.clone(),
            operator: current_token.literal,
            right: None,
            left: Some(left.to_owned()),
        };
        let precedence = parser.current_precedence();
        parser.next_token();
        expression.right = parser.parse_expression(precedence);
        return Some(Rc::new(Expression::InfixExpression(expression)));
    }
    None
}

pub fn parse_grouped_expression(parser: &mut Parser) -> Option<Rc<Expression>> {
    parser.next_token();
    let expression = parser.parse_expression(Precedence::LOWEST);
    if !parser.expect_peek(TokenType::RPAREN) {
        return None;
    }
    expression
}

pub fn parse_if_expression(parser: &mut Parser) -> Option<Rc<Expression>> {
    if let Some(token) = parser.current_token.clone() {
        if !parser.expect_peek(TokenType::LPAREN) {
            return None;
        }
        parser.next_token();
        let condition = parser.parse_expression(Precedence::LOWEST);

        if !parser.expect_peek(TokenType::RPAREN) {
            return None;
        }
        if !parser.expect_peek(TokenType::LBRACE) {
            return None;
        }

        let mut expression = IfExpression {
            token,
            condition,
            consequence: parse_block_statement(parser),
            alternative: None,
        };
        if parser.peek_token_is(TokenType::ELSE) {
            parser.next_token();
            if !parser.expect_peek(TokenType::LBRACE) {
                return None;
            }
            expression.alternative = parse_block_statement(parser);
        }

        return Some(Rc::new(Expression::IfExpression(expression)));
    }

    None
}

pub fn parse_function_literal(parser: &mut Parser) -> Option<Rc<Expression>> {
    if let Some(token) = parser.current_token.clone() {
        if !parser.expect_peek(TokenType::LPAREN) {
            return None;
        }
        let parameters = parse_function_paramenters(parser);
        if !parser.expect_peek(TokenType::LBRACE) {
            return None;
        }
        let body = parse_block_statement(parser);
        let mut expression = FunctionLiteral {
            token,
            parameters: Vec::new(),
            body,
        };

        if let Some(parameters) = parameters {
            expression.parameters = parameters;
        }
        return Some(Rc::new(Expression::FunctionLiteral(expression)));
    }
    None
}

pub fn parse_function_paramenters(parser: &mut Parser) -> Option<Vec<Identifier>> {
    let mut parameters: Vec<Identifier> = Vec::new();

    if parser.peek_token_is(TokenType::RPAREN) {
        parser.next_token();
        return Some(parameters);
    }

    parser.next_token();
    if let Some(token) = parser.current_token.clone() {
        let idnt = Identifier {
            token: token.clone(),
            value: token.literal,
        };
        parameters.push(idnt);

        while parser.peek_token_is(TokenType::COMMA) {
            parser.next_token();
            parser.next_token();
            if let Some(token) = parser.current_token.clone() {
                let idnt = Identifier {
                    token: token.clone(),
                    value: token.literal,
                };
                parameters.push(idnt);
            }
        }

        if !parser.expect_peek(TokenType::RPAREN) {
            return None;
        }

        return Some(parameters);
    }

    return Some(parameters);
}

fn parse_block_statement(parser: &mut Parser) -> Option<BlockStatement> {
    if let Some(token) = parser.current_token.clone() {
        let mut block_statement = BlockStatement {
            token,
            statements: Vec::new(),
        };
        parser.next_token();
        while !parser.current_token_is(TokenType::RBRACE)
            && !parser.current_token_is(TokenType::EOF)
        {
            let statement = parser.parse_statement();
            if let Some(statement) = statement {
                block_statement.statements.push(*statement);
            }
            parser.next_token();
        }
        return Some(block_statement);
    }

    None
}

pub fn parse_call_expression(
    parser: &mut Parser,
    function: Rc<Expression>,
) -> Option<Rc<Expression>> {
    if let Some(token) = parser.current_token.clone() {
        parser.next_token();
        let mut expression = CallExpression {
            function,
            token,
            args: Vec::new(),
        };
        if let Some(arguments) = parse_call_arguments(parser) {
            expression.args = arguments;
        }
        return Some(Rc::new(Expression::CallExpression(expression)));
    }
    None
}

pub fn parse_call_arguments(parser: &mut Parser) -> Option<Vec<Rc<Expression>>> {
    let mut args: Vec<Rc<Expression>> = Vec::new();
    if parser.peek_token_is(TokenType::RPAREN) {
        parser.next_token();
        return Some(args);
    }
    parser.next_token();
    if let Some(argument) = parser.parse_expression(Precedence::LOWEST) {
        args.push(argument);
        while parser.peek_token_is(TokenType::COMMA) {
            parser.next_token();
            parser.next_token();
            if let Some(argument) = parser.parse_expression(Precedence::LOWEST) {
                args.push(argument);
            }
        }
        if !parser.expect_peek(TokenType::RPAREN) {
            return None;
        }
        return Some(args);
    }
    return None;
}

pub fn parse_boolean(parser: &mut Parser) -> Option<Rc<Expression>> {
    if let Some(token) = &parser.current_token {
        return Some(Rc::new(Expression::Boolean(Boolean {
            token: token.clone(),
            value: parser.current_token_is(TokenType::TRUE),
        })));
    }
    None
}
