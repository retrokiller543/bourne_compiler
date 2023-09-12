// Module: lexer
// Purpose: lexing the input
// Path: src/lexer/mod.rs

#[derive(PartialEq, Debug, Clone)]
pub enum Token {
    Number(i32),
    String(String),
    Operator(Operator),
    Identifier(String),
    Keyword(Keyword),
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Assign,
    EoL,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Keyword {
    If,
    Else,
    ElseIf,
    While,
    For,
    Exit,
    Let,
    Print,
    Arg,
    Exec,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Operator {
    Plus,
    Minus,
    Multiply,
    Divide,
    Equal,
    NotEqual,
}

#[derive(Debug, Clone)]
pub struct Lexer {
    input: String,
}

macro_rules! match_number {
    ($chars:ident) => {{
        let mut number = String::new();
        while let Some(ch) = $chars.peek() {
            if ch.is_digit(10) {
                number.push($chars.next().unwrap());
            } else {
                break;
            }
        }
        if !number.is_empty() {
            vec![Token::Number(number.parse().unwrap())]
        } else {
            vec![]
        }
    }};
}

macro_rules! match_keyword {
    ($chars:ident) => {{
        let mut keyword = String::new();
        let mut chars_copy = $chars.clone();
        while let Some(ch) = chars_copy.peek() {
            if ch.is_alphanumeric() || *ch == '_' {
                keyword.push(chars_copy.next().unwrap());
            } else {
                break;
            }
        }
        match keyword.as_str() {
            "if" => {
                $chars = chars_copy;
                vec![Token::Keyword(Keyword::If)]
            },
            "else" => {
                $chars = chars_copy;
                vec![Token::Keyword(Keyword::Else)]
            },
            "elseif" => {
                $chars = chars_copy;
                vec![Token::Keyword(Keyword::ElseIf)]
            },
            "while" => {
                $chars = chars_copy;
                vec![Token::Keyword(Keyword::While)]
            },
            "for" => {
                $chars = chars_copy;
                vec![Token::Keyword(Keyword::For)]
            },
            "exit" => {
                $chars = chars_copy;
                vec![Token::Keyword(Keyword::Exit)]
            },
            "let" => {
                $chars = chars_copy;
                vec![Token::Keyword(Keyword::Let)]
            },
            "print" => {
                $chars = chars_copy;
                vec![Token::Keyword(Keyword::Print)]
            },
            "arg" => {
                $chars = chars_copy;
                vec![Token::Keyword(Keyword::Arg)]
            },
            "exec" => {
                $chars = chars_copy;
                vec![Token::Keyword(Keyword::Exec)]
            },
            
            _ => vec![],
        }
    }};
}

macro_rules! match_identifier {
    ($chars:ident) => {{
        let mut identifier = String::new();
        while let Some(ch) = $chars.peek() {
            if ch.is_alphanumeric() || *ch == '_' {
                identifier.push($chars.next().unwrap());
            } else {
                break;
            }
        }
        if !identifier.is_empty() {
            vec![Token::Identifier(identifier)]
        } else {
            vec![]
        }
    }};
}

macro_rules! match_string {
    ($chars:ident) => {{
        let mut string_val = String::new();
        if let Some(&'"') = $chars.peek() {
            $chars.next(); // Consume the opening quote
            while let Some(&ch) = $chars.peek() {
                if ch != '"' {
                    string_val.push($chars.next().unwrap());
                } else {
                    $chars.next(); // Consume the closing quote
                    break;
                }
            }
            vec![Token::String(string_val)]
        } else {
            vec![]
        }
    }};
}

macro_rules! match_token {
    ($chars:ident, $( ($token_pattern: pat, $token_expr: expr) ),* , numbers, keywords, identifiers) => {
        match $chars.peek() {
            Some(&'=') => {
                $chars.next(); // Consume first '='
                match $chars.peek() {
                    Some(&'=') => {
                        $chars.next(); // Consume second '='
                        vec![Token::Operator(Operator::Equal)]
                    }
                    _ => vec![Token::Assign],
                }
            }
            Some(&'!') => {
                $chars.next(); // Consume '!'
                match $chars.peek() {
                    Some(&'=') => {
                        $chars.next(); // Consume '='
                        vec![Token::Operator(Operator::NotEqual)]
                    }
                    _ => vec![], // Handle unexpected token or return error
                }
            }
            Some(&ch) => match ch {
                '"' => match_string!($chars),
                $($token_pattern => {
                    $chars.next();
                    vec![$token_expr.unwrap()]
                })*
                _ => {
                    let mut tokens = match_number!($chars);
                    if tokens.is_empty() {
                        tokens = match_keyword!($chars);
                    }
                    if tokens.is_empty() {
                        tokens = match_identifier!($chars);
                    }
                    tokens
                }
            }
            _ => vec![],
        }
    };
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        Lexer {
            input: input.to_string(),
        }
    }

    pub fn lex(&self) -> Vec<Token> {
        let mut tokens = Vec::new();
        let mut chars = self.input.chars().peekable();

        while chars.peek().is_some() {
            let matched_tokens = match_token!(
                chars,
                ('+', Some(Token::Operator(Operator::Plus))),
                ('-', Some(Token::Operator(Operator::Minus))),
                ('*', Some(Token::Operator(Operator::Multiply))),
                ('/', Some(Token::Operator(Operator::Divide))),
                ('=', Some(Token::Assign)),
                ('(', Some(Token::OpenParen)),
                (')', Some(Token::CloseParen)),
                ('{', Some(Token::OpenBrace)),
                ('}', Some(Token::CloseBrace)),
                (';', Some(Token::EoL)),
                numbers,
                keywords,
                identifiers
            );

            if matched_tokens.is_empty() {
                chars.next();  // Skip unrecognized characters
            } else {
                tokens.extend(matched_tokens);
            }
        }

        tokens
    }
}

#[cfg(test)]
mod tests {
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
    test_lexer!(variable_definition, "let x = 123;", vec![
        Token::Keyword(Keyword::Let),
        Token::Identifier("x".to_string()),
        Token::Assign,
        Token::Number(123),
        Token::EoL
    ]);

    test_lexer!(if_statement, "if (x == 123) { x = 456; }", vec![
        Token::Keyword(Keyword::If),
        Token::OpenParen,
        Token::Identifier("x".to_string()),
        Token::Operator(Operator::Equal),  // Update this
        Token::Number(123),
        Token::CloseParen,
        Token::OpenBrace,
        Token::Identifier("x".to_string()),
        Token::Assign,
        Token::Number(456),
        Token::EoL,
        Token::CloseBrace,
    ]);

    test_lexer!(equality_operator, "x == y", vec![
        Token::Identifier("x".to_string()),
        Token::Operator(Operator::Equal),
        Token::Identifier("y".to_string()),
    ]);

    test_lexer!(inequality_operator, "x != y", vec![
        Token::Identifier("x".to_string()),
        Token::Operator(Operator::NotEqual),
        Token::Identifier("y".to_string()),
    ]);

    test_lexer!(test_while, "while (x == 5) { let x = x + 1; }", vec![
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
    ]);

    test_lexer!(test_string, "\"Hello, World!\"", vec![
    Token::String("Hello, World!".to_string())
    ]);

    test_lexer!(test_exec, "exec(\"ls\")", vec![
        Token::Keyword(Keyword::Exec),
        Token::OpenParen,
        Token::String("ls".to_string()),
        Token::CloseParen
    ]);


}
