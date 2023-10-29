pub mod ast_nodes;
pub mod parser_functions;
pub mod precedence;
use std::{collections::HashMap, rc::Rc};

use crate::{
    lexer::Lexer,
    token::{Token, TokenType},
};

use self::{
    ast_nodes::{
        expressions::{Expression, Identifier},
        program::Program,
        statements::{ExpressionStatement, LetStatement, ReturnStatement, Statement},
        InfixParseFn, PrefixParseFn,
    },
    parser_functions::{
        parse_boolean, parse_call_expression, parse_function_literal, parse_grouped_expression,
        parse_identifier, parse_if_expression, parse_infix_expression, parse_integer_literal,
        parse_prefix_expression,
    },
    precedence::Precedence,
};

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Option<Token>,
    peek_token: Option<Token>,
    pub errors: Vec<String>,
    prefix_parse_fns: HashMap<TokenType, PrefixParseFn>,
    infix_parse_fns: HashMap<TokenType, InfixParseFn>,
    precedences_table: HashMap<TokenType, Precedence>,
}

impl Parser<'_> {
    pub fn new(mut lexer: Lexer) -> Parser {
        let peek_token = Some(lexer.next_token());
        let mut parser = Parser {
            lexer,
            peek_token,
            current_token: None,
            errors: Vec::new(),
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
            precedences_table: HashMap::new(),
        };
        parser.register_parser_functions();
        parser.init_precedences_table();
        parser.next_token();
        parser
    }

    pub fn register_parser_functions(&mut self) {
        //prefix functions
        self.register_prefix(TokenType::IDENT, parse_identifier);
        self.register_prefix(TokenType::INT, parse_integer_literal);
        self.register_prefix(TokenType::BANG, parse_prefix_expression);
        self.register_prefix(TokenType::MINUS, parse_prefix_expression);
        self.register_prefix(TokenType::TRUE, parse_boolean);
        self.register_prefix(TokenType::FALSE, parse_boolean);
        self.register_prefix(TokenType::LPAREN, parse_grouped_expression);
        self.register_prefix(TokenType::IF, parse_if_expression);
        self.register_prefix(TokenType::FUNCTION, parse_function_literal);

        //Infix functions
        self.register_infix(TokenType::PLUS, parse_infix_expression);
        self.register_infix(TokenType::MINUS, parse_infix_expression);
        self.register_infix(TokenType::SLASH, parse_infix_expression);
        self.register_infix(TokenType::ASTERISK, parse_infix_expression);
        self.register_infix(TokenType::EQ, parse_infix_expression);
        self.register_infix(TokenType::NOTEQ, parse_infix_expression);
        self.register_infix(TokenType::LT, parse_infix_expression);
        self.register_infix(TokenType::GT, parse_infix_expression);
        self.register_infix(TokenType::LPAREN, parse_call_expression);
    }

    pub fn init_precedences_table(&mut self) {
        self.precedences_table
            .insert(TokenType::EQ, Precedence::EQUALS);
        self.precedences_table
            .insert(TokenType::NOTEQ, Precedence::EQUALS);
        self.precedences_table
            .insert(TokenType::LT, Precedence::LESSGREATER);
        self.precedences_table
            .insert(TokenType::GT, Precedence::LESSGREATER);
        self.precedences_table
            .insert(TokenType::PLUS, Precedence::SUM);
        self.precedences_table
            .insert(TokenType::MINUS, Precedence::SUM);
        self.precedences_table
            .insert(TokenType::SLASH, Precedence::PRODUCT);
        self.precedences_table
            .insert(TokenType::ASTERISK, Precedence::PRODUCT);
        self.precedences_table
            .insert(TokenType::LPAREN, Precedence::CALL);
    }

    pub fn register_prefix(&mut self, token_type: TokenType, function: PrefixParseFn) {
        self.prefix_parse_fns.insert(token_type, function);
    }
    pub fn register_infix(&mut self, token_type: TokenType, function: InfixParseFn) {
        self.infix_parse_fns.insert(token_type, function);
    }

    fn next_token(&mut self) {
        let peek_token = &self.peek_token;
        match peek_token {
            None => {
                self.peek_token = Some(self.lexer.next_token());
            }
            Some(token) => {
                self.current_token = Some(Token::new(token.token_type, token.literal.clone()));
                self.peek_token = Some(self.lexer.next_token());
            }
        }
    }

    pub fn parse_statement(&mut self) -> Option<Box<Statement>> {
        match &self.current_token {
            None => None,
            Some(token) => match token.token_type {
                TokenType::LET => self.parse_let_statement(),
                TokenType::RETURN => self.parse_return_statement(),
                _ => self.parse_expression_statement(),
            },
        }
    }

    pub fn parse_expression_statement(&mut self) -> Option<Box<Statement>> {
        if let Some(current_token) = &self.current_token {
            let token = current_token.clone();
            let statement_exp = self.parse_expression(Precedence::LOWEST);

            let statement = ExpressionStatement {
                token,
                expression: statement_exp,
            };

            if self.peek_token_is(TokenType::SEMICOLON) {
                self.next_token();
            }
            Some(Box::new(Statement::ExpressionStatement(statement)))
        } else {
            None
        }
    }

    pub fn parse_expression(&mut self, precedence: Precedence) -> Option<Rc<Expression>> {
        if let Some(token) = &self.current_token {
            let prefix_fn = self.prefix_parse_fns.get_mut(&token.token_type);

            if let None = prefix_fn {
                self.no_prefix_parse_fn_error(token.token_type);
                return None;
            } else if let Some(function) = prefix_fn {
                let mut left_expression = function(self);
                while !self.peek_token_is(TokenType::SEMICOLON)
                    && precedence < self.peek_precedence()
                {
                    if let Some(peek_token) = &self.peek_token {
                        let infix_opt = self.infix_parse_fns.get(&peek_token.token_type);
                        if let None = infix_opt {
                            return left_expression;
                        } else if let Some(infix) = infix_opt {
                            match left_expression {
                                None => (),
                                Some(expression) => {
                                    left_expression = infix(self, expression);
                                }
                            }
                        }
                    };
                }
                return left_expression;
            }
            return None;
        }
        None
    }

    pub fn no_prefix_parse_fn_error(&mut self, token_type: TokenType) {
        let message = String::from(format!("No prefix function for {:?} found", token_type));
        self.errors.push(message);
    }

    pub fn parse_return_statement(&mut self) -> Option<Box<Statement>> {
        if let Some(current_token) = &self.current_token {
            let return_token = current_token.clone();
            self.next_token();
            let mut return_statement = ReturnStatement {
                token: return_token,
                return_value: None,
            };
            if let Some(return_value) = self.parse_expression(Precedence::LOWEST) {
                return_statement.return_value = Some(return_value);
            }
            if self.peek_token_is(TokenType::SEMICOLON) {
                self.next_token();
            }
            return Some(Box::new(Statement::ReturnStatement(return_statement)));
        } else {
            None
        }
    }

    pub fn parse_let_statement(&mut self) -> Option<Box<Statement>> {
        if let Some(current_token) = &self.current_token {
            let let_token = current_token.clone();

            if !self.expect_peek(TokenType::IDENT) {
                return None;
            }

            let statement_name_token = match &self.current_token {
                Some(token) => token,
                None => {
                    return None;
                }
            };

            let statement_name = Identifier {
                token: statement_name_token.clone(),
                value: statement_name_token.literal.clone(),
            };

            if !self.expect_peek(TokenType::ASSIGN) {
                return None;
            }

            self.next_token();

            let mut statement = LetStatement {
                token: let_token,
                name: statement_name,
                value: None,
            };

            if let Some(value) = self.parse_expression(Precedence::LOWEST) {
                statement.value = Some(value);
            }

            if self.peek_token_is(TokenType::SEMICOLON) {
                self.next_token();
            }

            Some(Box::new(Statement::LetStatement(statement)))
        } else {
            None
        }
    }

    pub fn expect_peek(&mut self, token_type: TokenType) -> bool {
        match self.peek_token_is(token_type) {
            true => {
                self.next_token();
                true
            }
            false => {
                self.peek_error(token_type);
                false
            }
        }
    }

    pub fn current_token_is(&mut self, token_type: TokenType) -> bool {
        if let Some(token) = &self.current_token {
            return token.token_type == token_type;
        }
        false
    }

    pub fn peek_token_is(&mut self, token_type: TokenType) -> bool {
        if let Some(token) = &self.peek_token {
            return token.token_type == token_type;
        }
        false
    }

    pub fn current_precedence(&mut self) -> Precedence {
        if let Some(token) = &self.current_token {
            match self.precedences_table.get(&token.token_type) {
                None => {
                    return Precedence::LOWEST;
                }
                Some(value) => {
                    return value.clone();
                }
            }
        }
        Precedence::LOWEST
    }

    pub fn peek_precedence(&mut self) -> Precedence {
        if let Some(token) = &self.peek_token {
            match self.precedences_table.get(&token.token_type) {
                None => {
                    return Precedence::LOWEST;
                }
                Some(value) => {
                    return value.clone();
                }
            }
        }
        Precedence::LOWEST
    }

    pub fn peek_error(&mut self, token_type: TokenType) {
        match &self.peek_token {
            None => panic!("Peek token should exist"),
            Some(token) => {
                let error_str = String::from(format!(
                    "Expected next token to be {:?}, got {:?} instead",
                    token_type, token.token_type
                ));
                self.errors.push(error_str.to_string());
            }
        }
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program::new();
        program.statements = Vec::new();

        while let Some(token) = &self.current_token {
            if token.token_type == TokenType::EOF {
                break;
            }
            if token.token_type == TokenType::ILLEGAL {
                self.errors.push(format!("Illegal token recieved: {:?}", token.literal));
                 break;
            }
            let statement = self.parse_statement();
            if let Some(statemet) = statement {
                program.statements.push(*statemet);
            }
            self.next_token();
        }
        program
    }
}

#[cfg(test)]
mod tests {

    use std::rc::Rc;

    use crate::{
        lexer::Lexer,
        parser::ast_nodes::{program::Program, Node},
        token::{Token, TokenType},
    };

    use super::{
        ast_nodes::{
            expressions::{Expression, Identifier},
            statements::{LetStatement, Statement},
        },
        Parser,
    };

    pub enum Expected {
        IntExpected(i64),
        StringExpected(String),
    }

    fn test_integer_literal(exp: &Option<Rc<Expression>>, value: i64) {
        match exp {
            None => panic!("No expression, got None"),
            Some(exp) => match exp.as_ref() {
                Expression::IntegerLiteral(integer_literal) => {
                    assert_eq!(integer_literal.value, value);
                    assert_eq!(format!("{}", value), integer_literal.token_literal())
                }
                _ => panic!("Expecting an IntegerLiteral, got {:?}", exp),
            },
        }
    }

    pub fn test_identifier(exp: &Option<Rc<Expression>>, value: &str) {
        match exp {
            None => panic!("No expression, got None"),
            Some(exp) => match exp.as_ref() {
                Expression::Identifier(identifier) => {
                    assert_eq!(identifier.value, value);
                    assert_eq!(format!("{}", value), identifier.token_literal())
                }
                _ => panic!("Expecting an Identifier, got {:?}", exp),
            },
        }
    }

    pub fn test_literal_expression(exp: &Option<Rc<Expression>>, expected: Expected) {
        match expected {
            Expected::IntExpected(value) => test_integer_literal(exp, value),
            Expected::StringExpected(value) => test_identifier(exp, &value),
        }
    }

    pub fn test_infix_expression(
        exp: &Option<Rc<Expression>>,
        left: Expected,
        operator: &str,
        right: Expected,
    ) {
        match exp {
            None => panic!("Not an expression, got None"),
            Some(expression) => match expression.as_ref() {
                Expression::InfixExpression(op_exp) => {
                    test_literal_expression(&op_exp.left, left);
                    assert_eq!(op_exp.operator, operator);
                    test_literal_expression(&op_exp.right, right);
                }
                _ => panic!("Expected an InfixExpression, got {:?}", expression),
            },
        }
    }

    pub fn test_let_statement(statement: &Statement, expected_identifier: &str) -> bool {
        let token_literal = statement.token_literal();
        if token_literal != "let" {
            panic!("s.TokenLiteral not 'let'. got={}", token_literal);
        }
        match statement {
            Statement::LetStatement(let_statement) => {
                if let_statement.name.value != expected_identifier {
                    panic!(
                        "let_statement.name.value not {}, got {}",
                        expected_identifier, let_statement.name.value
                    );
                };
                let token_literal = let_statement.name.token_literal();
                if token_literal != expected_identifier {
                    panic!(
                        "let_statement.name.token_literal() not {}, got {}",
                        expected_identifier, token_literal
                    );
                }
            }
            _ => panic!("Expecting an IntegerLiteral, got {:?}", statement),
        }
        true
    }

    pub fn check_parser_errors(parser: &mut Parser) {
        let errors = &parser.errors;
        if errors.len() == 0 {
            return;
        }
        for error in errors {
            println!("Parsing error: {}", error);
        }
        panic!("There are parsing errors!");
    }

    #[test]
    fn it_should_parse_let_statements() {
        let input = "
            let x = 5;
            let y = 10;
            let foobar = 838383;";
        let lexer = Lexer::new(input.as_bytes());
        let mut parser = Parser::new(lexer);
        check_parser_errors(&mut parser);
        let program = parser.parse_program();
        let statements_len = program.statements.len();
        if statements_len != 3 {
            panic!(
                "Program.statements does not contain 3 statements, got {}",
                statements_len
            );
        }

        let expected_identifiers = vec!["x", "y", "foobar"];
        for (i, expected_identifier) in expected_identifiers.iter().enumerate() {
            let statement = &program.statements[i];
            if !test_let_statement(statement, &expected_identifier) {
                return;
            };
        }
    }

    #[test]
    fn it_should_parse_return_statements() {
        let input = "
        return 5;
        return 10;
        return 993322;
        ";
        let lexer = Lexer::new(input.as_bytes());
        let mut parser = Parser::new(lexer);
        check_parser_errors(&mut parser);
        let program = parser.parse_program();
        let statements_len = program.statements.len();
        if statements_len != 3 {
            panic!(
                "Program.statements does not contain 3 statements, got {}",
                statements_len
            );
        }

        for boxed in program.statements {
            match boxed {
                Statement::ReturnStatement(statement) => {
                    let token_literal = statement.token_literal();
                    if token_literal != "return" {
                        panic!(
                            "return_statement.token_literal() not 'return', got {}",
                            token_literal
                        );
                    }
                }
                _ => panic!("Not a return Statement: {:?}", boxed),
            }
        }
    }

    #[test]
    pub fn it_should_build_program_string() {
        let program = Program {
            statements: vec![Statement::LetStatement(LetStatement {
                token: Token {
                    token_type: TokenType::LET,
                    literal: "let".to_string(),
                },
                name: Identifier {
                    token: Token {
                        token_type: TokenType::IDENT,
                        literal: "some_var".to_string(),
                    },
                    value: "some_var".to_string(),
                },
                value: Some(Rc::new(Expression::Identifier(Identifier {
                    token: Token {
                        token_type: TokenType::IDENT,
                        literal: "other_var".to_string(),
                    },
                    value: "other_var".to_string(),
                }))),
            })],
        };

        assert_eq!(program.string(), "let some_var=other_var;")
    }

    #[test]
    fn it_should_parse_identifier_expressions() {
        let input = "foobar;";
        let lexer = Lexer::new(input.as_bytes());
        let mut parser = Parser::new(lexer);
        check_parser_errors(&mut parser);
        let program = parser.parse_program();
        assert_eq!(1, program.statements.len());
        if let Statement::ExpressionStatement(expression) = &program.statements[0] {
            if let Expression::Identifier(identifier) = expression
                .expression
                .as_ref()
                .expect("Received None!")
                .as_ref()
            {
                assert_eq!(identifier.value, "foobar");
                assert_eq!(identifier.token_literal(), "foobar");
            } else {
                panic!("Not an identifier");
            }
        } else {
            panic!("Not an ExpressionStatement");
        }
    }

    #[test]
    fn it_should_parse_integer_expressions() {
        let input = "5;";
        let lexer = Lexer::new(input.as_bytes());
        let mut parser = Parser::new(lexer);
        check_parser_errors(&mut parser);
        let program = parser.parse_program();
        assert_eq!(1, program.statements.len());

        if let Statement::ExpressionStatement(expression) = &program.statements[0] {
            if let Expression::IntegerLiteral(identifier) = expression
                .expression
                .as_ref()
                .expect("Received None!")
                .as_ref()
            {
                assert_eq!(identifier.value, 5);
                assert_eq!(identifier.token_literal(), "5");
            } else {
                panic!("Not an IntegerLiteral");
            }
        } else {
            panic!("Not an ExpressionStatement");
        }
    }

    #[test]
    fn it_should_parse_prefix_expressions() {
        struct PrefixTest<'a> {
            pub exp: &'a str,
            pub operator: &'a str,
            pub value: i64,
        }

        let test_data = vec![
            PrefixTest {
                exp: "!5;",
                operator: "!",
                value: 5,
            },
            PrefixTest {
                exp: "-15;",
                operator: "-",
                value: 15,
            },
        ];

        for test in test_data {
            let lexer = Lexer::new(test.exp.as_bytes());
            let mut parser = Parser::new(lexer);
            check_parser_errors(&mut parser);
            let program = parser.parse_program();
            assert_eq!(1, program.statements.len());

            if let Statement::ExpressionStatement(expression) = &program.statements[0] {
                if let Expression::PrefixExpression(identifier) = expression
                    .expression
                    .as_ref()
                    .expect("Received None!")
                    .as_ref()
                {
                    assert_eq!(identifier.operator, test.operator);
                    test_literal_expression(&identifier.right, Expected::IntExpected(test.value));
                } else {
                    panic!("Not an IntegerLiteral");
                }
            } else {
                panic!("Not an ExpressionStatement");
            }
        }
    }

    #[test]
    fn it_should_parse_infix_expressions() {
        struct InfixTest {
            pub input: String,
            pub left_value: Expected,
            pub operator: String,
            pub rigth_value: Expected,
        }
        impl InfixTest {
            fn new(
                input: String,
                left_value: Expected,
                operator: String,
                rigth_value: Expected,
            ) -> InfixTest {
                return InfixTest {
                    input,
                    left_value,
                    operator,
                    rigth_value,
                };
            }
        }
        let tests = vec![
            InfixTest::new(
                "5 + 5;".to_string(),
                Expected::IntExpected(5),
                "+".to_string(),
                Expected::IntExpected(5),
            ),
            InfixTest::new(
                "5 - 5;".to_string(),
                Expected::IntExpected(5),
                "-".to_string(),
                Expected::IntExpected(5),
            ),
            InfixTest::new(
                "5 * 5;".to_string(),
                Expected::IntExpected(5),
                "*".to_string(),
                Expected::IntExpected(5),
            ),
            InfixTest::new(
                "5 / 5;".to_string(),
                Expected::IntExpected(5),
                "/".to_string(),
                Expected::IntExpected(5),
            ),
            InfixTest::new(
                "5 > 5;".to_string(),
                Expected::IntExpected(5),
                ">".to_string(),
                Expected::IntExpected(5),
            ),
            InfixTest::new(
                "5 < 5;".to_string(),
                Expected::IntExpected(5),
                "<".to_string(),
                Expected::IntExpected(5),
            ),
            InfixTest::new(
                "5 == 5;".to_string(),
                Expected::IntExpected(5),
                "==".to_string(),
                Expected::IntExpected(5),
            ),
            InfixTest::new(
                "A != B;".to_string(),
                Expected::StringExpected("A".to_string()),
                "!=".to_string(),
                Expected::StringExpected("B".to_string()),
            ),
        ];

        for test in tests {
            let lexer = Lexer::new(test.input.as_bytes());
            let mut parser = Parser::new(lexer);
            check_parser_errors(&mut parser);
            let program = parser.parse_program();
            assert_eq!(1, program.statements.len());

            if let Statement::ExpressionStatement(statement) = &program.statements[0] {
                let boxed = &statement.expression;
                test_infix_expression(boxed, test.left_value, &test.operator, test.rigth_value);
            }
        }
    }

    #[test]
    fn it_should_parse_grouped_expressions() {
        struct OperatorPrecedenceTest<'a> {
            input: &'a str,
            expected: &'a str,
        }

        impl<'a> OperatorPrecedenceTest<'a> {
            fn new(input: &'a str, expected: &'a str) -> Self {
                OperatorPrecedenceTest { input, expected }
            }
        }
        let tests = vec![
            OperatorPrecedenceTest::new("-a * b", "((-a) * b)"),
            OperatorPrecedenceTest::new("!-a", "(!(-a))"),
            OperatorPrecedenceTest::new("a + b + c", "((a + b) + c)"),
            OperatorPrecedenceTest::new("a + b - c", "((a + b) - c)"),
            OperatorPrecedenceTest::new("a * b * c", "((a * b) * c)"),
            OperatorPrecedenceTest::new("a * b / c", "((a * b) / c)"),
            OperatorPrecedenceTest::new("a + b / c", "(a + (b / c))"),
            OperatorPrecedenceTest::new("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            OperatorPrecedenceTest::new("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            OperatorPrecedenceTest::new("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            OperatorPrecedenceTest::new("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            OperatorPrecedenceTest::new(
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
        ];

        for test in tests {
            let lexer = Lexer::new(test.input.as_bytes());
            let mut parser = Parser::new(lexer);
            check_parser_errors(&mut parser);
            let program = parser.parse_program();
            assert_eq!(program.string(), test.expected);
        }
    }

    #[test]
    fn it_should_parse_function_literals() {
        let input = "fn(x, y) { x + y; }";
        let lexer = Lexer::new(input.as_bytes());
        let mut parser = Parser::new(lexer);
        check_parser_errors(&mut parser);
        let program = parser.parse_program();
        assert_eq!(1, program.statements.len());

        if let Statement::ExpressionStatement(statement) = &program.statements[0] {
            if let Expression::FunctionLiteral(function_literal) = statement
                .expression
                .as_ref()
                .expect("Received None!")
                .as_ref()
            {
                assert_eq!(function_literal.parameters.len(), 2);
                let param1 = function_literal.parameters[0].clone();
                let param2 = function_literal.parameters[1].clone();
                test_literal_expression(
                    &Some(Rc::new(Expression::Identifier(param1))),
                    Expected::StringExpected('x'.to_string()),
                );
                test_literal_expression(
                    &Some(Rc::new(Expression::Identifier(param2))),
                    Expected::StringExpected('y'.to_string()),
                );
                assert_eq!(
                    function_literal
                        .body
                        .as_ref()
                        .expect("None instead of body")
                        .statements
                        .len(),
                    1
                );

                let stmt = &function_literal
                    .body
                    .as_ref()
                    .expect("Not a blockStatement")
                    .statements[0];
                if let Statement::ExpressionStatement(stmt) = stmt {
                    test_infix_expression(
                        &stmt.expression,
                        Expected::StringExpected("x".to_string()),
                        "+",
                        Expected::StringExpected("y".to_string()),
                    )
                }
            } else {
                panic!("Not a FunctionLiteral")
            }
        }
    }

    #[test]
    fn it_should_parse_call_expressions() {
        let input = "add(1, 2 * 3, 4 + 5);";
        let lexer = Lexer::new(input.as_bytes());
        let mut parser = Parser::new(lexer);
        check_parser_errors(&mut parser);
        let program = parser.parse_program();
        assert_eq!(1, program.statements.len());

        if let Statement::ExpressionStatement(statement) = &program.statements[0] {
            if let Expression::CallExpression(call_expression) = statement
                .expression
                .as_ref()
                .expect("Recieved None!")
                .as_ref()
            {
                test_identifier(&Some(call_expression.function.clone()), "add");
                assert_eq!(call_expression.args.len(), 3);
                test_literal_expression(
                    &Some(call_expression.args[0].clone()),
                    Expected::IntExpected(1),
                );
                test_infix_expression(
                    &Some(call_expression.args[1].clone()),
                    Expected::IntExpected(2),
                    "*",
                    Expected::IntExpected(3),
                );
                test_infix_expression(
                    &Some(call_expression.args[2].clone()),
                    Expected::IntExpected(4),
                    "+",
                    Expected::IntExpected(5),
                );
            }
        }
    }

    #[test]
    fn it_should_parse_if_expressions() {
        let input = "if (x < y) { x }";
        let lexer = Lexer::new(input.as_bytes());
        let mut parser = Parser::new(lexer);
        check_parser_errors(&mut parser);
        let program = parser.parse_program();
        assert_eq!(1, program.statements.len());

        if let Statement::ExpressionStatement(statement) = &program.statements[0] {
            if let Expression::IfExpression(if_expression) = statement
                .expression
                .as_ref()
                .expect("Recieved None!")
                .as_ref()
            {
                test_infix_expression(
                    &if_expression.condition,
                    Expected::StringExpected("x".to_string()),
                    "<",
                    Expected::StringExpected("y".to_string()),
                );

                let consequence = if_expression
                    .consequence
                    .as_ref()
                    .expect("None insteat of consequence");
                if let Statement::ExpressionStatement(stmt) = &consequence.statements[0] {
                    test_literal_expression(
                        &stmt.expression,
                        Expected::StringExpected("x".to_string()),
                    );
                }
            }
        }
    }
}
