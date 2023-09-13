use super::*;

macro_rules! test_lexer {
    ($name:ident, $input:expr, $expected_output:expr) => {
        #[test]
        fn $name() {
            let lexer = Lexer::new($input);
            assert_eq!(lexer.lex(), $expected_output);
        }
    };
}

test_lexer!(empty, "", vec![]);

test_lexer!(plus, "+", vec![Token::Operator(Operator::Plus)]);

test_lexer!(minus, "-", vec![Token::Operator(Operator::Minus)]);

test_lexer!(multiply, "*", vec![Token::Operator(Operator::Multiply)]);

test_lexer!(divide, "/", vec![Token::Operator(Operator::Divide)]);

test_lexer!(assign, "=", vec![Token::Assign]);

test_lexer!(eol, ";", vec![Token::EoL]);

test_lexer!(number, "123", vec![Token::Number(123)]);

test_lexer!(
    identifier,
    "abc",
    vec![Token::Identifier("abc".to_string())]
);

test_lexer!(
    identifier_with_number,
    "abc123",
    vec![Token::Identifier("abc123".to_string())]
);

test_lexer!(
    identifier_with_underscore,
    "abc_123",
    vec![Token::Identifier("abc_123".to_string())]
);

test_lexer!(
    complex_expr,
    "variable_name = 123",
    vec![
        Token::Identifier("variable_name".to_string()),
        Token::Assign,
        Token::Number(123)
    ]
);

test_lexer!(
    complex_expr_with_paren,
    "variable_name = (123 + 456) * 789",
    vec![
        Token::Identifier("variable_name".to_string()),
        Token::Assign,
        Token::OpenParen,
        Token::Number(123),
        Token::Operator(Operator::Plus),
        Token::Number(456),
        Token::CloseParen,
        Token::Operator(Operator::Multiply),
        Token::Number(789)
    ]
);

test_lexer!(
    complex_expr_with_paren_and_identifier,
    "variable_name = (123 + variable_name) * 789",
    vec![
        Token::Identifier("variable_name".to_string()),
        Token::Assign,
        Token::OpenParen,
        Token::Number(123),
        Token::Operator(Operator::Plus),
        Token::Identifier("variable_name".to_string()),
        Token::CloseParen,
        Token::Operator(Operator::Multiply),
        Token::Number(789)
    ]
);
test_lexer!(
    variable_definition,
    "let x = 123;",
    vec![
        Token::Keyword(Keyword::Let),
        Token::Identifier("x".to_string()),
        Token::Assign,
        Token::Number(123),
        Token::EoL
    ]
);

test_lexer!(
    if_statement,
    "if (x == 123) { x = 456; }",
    vec![
        Token::Keyword(Keyword::If),
        Token::OpenParen,
        Token::Identifier("x".to_string()),
        Token::Operator(Operator::Equal),
        Token::Number(123),
        Token::CloseParen,
        Token::OpenBrace,
        Token::Identifier("x".to_string()),
        Token::Assign,
        Token::Number(456),
        Token::EoL,
        Token::CloseBrace,
    ]
);

test_lexer!(
    equality_operator,
    "x == y",
    vec![
        Token::Identifier("x".to_string()),
        Token::Operator(Operator::Equal),
        Token::Identifier("y".to_string()),
    ]
);

test_lexer!(
    inequality_operator,
    "x != y",
    vec![
        Token::Identifier("x".to_string()),
        Token::Operator(Operator::NotEqual),
        Token::Identifier("y".to_string()),
    ]
);

test_lexer!(
    test_while,
    "while (x == 5) { let x = x + 1; }",
    vec![
        Token::Keyword(Keyword::While),
        Token::OpenParen,
        Token::Identifier("x".to_string()),
        Token::Operator(Operator::Equal),
        Token::Number(5),
        Token::CloseParen,
        Token::OpenBrace,
        Token::Keyword(Keyword::Let),
        Token::Identifier("x".to_string()),
        Token::Assign,
        Token::Identifier("x".to_string()),
        Token::Operator(Operator::Plus),
        Token::Number(1),
        Token::EoL,
        Token::CloseBrace,
    ]
);

test_lexer!(
    test_string,
    "\"Hello, World!\"",
    vec![Token::StringLiteral("Hello, World!".to_string())]
);

test_lexer!(
    test_exec,
    "exec(\"ls\")",
    vec![
        Token::BuiltInFunction(BuiltInFunction::Exec),
        Token::OpenParen,
        Token::StringLiteral("ls".to_string()),
        Token::CloseParen
    ]
);
