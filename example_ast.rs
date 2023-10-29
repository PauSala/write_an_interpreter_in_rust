/*
ExpressionStatement {
    token: Token { token_type: INT, literal: "3" },
    expression: Some(InfixExpression {
        token: Token { token_type: MINUS, literal: "-" },
        left: Some(IntegerLiteral { token: Token { token_type: INT, literal: "3" }, value: 3 }),
        operator: "-",
        right: Some(InfixExpression {
            token: Token { token_type: ASTERISK, literal: "*" },
            left: Some(IntegerLiteral { token: Token { token_type: INT, literal: "5" }, value: 5 }),
            operator: "*",
            right: Some(IntegerLiteral {
                token: Token { token_type: INT, literal: "2" },
                value: 2,
            }),
        }),
    }),
};
*/
