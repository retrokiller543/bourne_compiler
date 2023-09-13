// Module: lexer
// Purpose: lexing the input
// Path: src/lexer/mod.rs

//! # Lexer Module
//!
//! The lexer's purpose is to tokenize the input. Tokenization is the process of converting a sequence
//! of characters into a sequence of tokens. These tokens are then used by the parser to generate
//! an Abstract Syntax Tree (AST).
//!
//! The lexer uses a combination of enumerations and macros to identify and categorize the different
//! parts of the input.
//!
//! Path: src/lexer/mod.rs

/// Represents the different types of tokens that can be identified by the lexer.
#[derive(PartialEq, Debug, Clone)]
pub enum Token {
    Number(i32),
    StringLiteral(String),
    Operator(Operator),
    Identifier(String),
    Keyword(Keyword),
    BuiltInFunction(BuiltInFunction),
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Comma,
    Assign,
    EoL,
}

/// Represents reserved keywords in the language.
#[derive(PartialEq, Debug, Clone)]
pub enum Keyword {
    If,
    Else,
    ElseIf,
    While,
    For,
    Let,
}

/// Represents built-in functions provided by the language.
#[derive(PartialEq, Debug, Clone)]
pub enum BuiltInFunction {
    Print,
    Arg,
    Exec,
    Exit,
}

/// Represents operators in the language.
#[derive(PartialEq, Debug, Clone)]
pub enum Operator {
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,
    Equal,
    NotEqual,
    GreaterThan,
    LessThan,
    GreaterThanOrEqual,
    LessThanOrEqual,
}

/// The `Lexer` struct holds the input string and provides the functionality to tokenize it.
#[derive(Debug, Clone)]
pub struct Lexer {
    input: String,
}

// Macros to match specific types of tokens. These are used to simplify the tokenization logic.

/// Matches numeric literals.
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

/// Matches built-in function names.
macro_rules! match_builtin_function {
    ($chars:ident) => {{
        let mut function_name = String::new();
        let mut chars_copy = $chars.clone();
        while let Some(ch) = chars_copy.peek() {
            if ch.is_alphanumeric() || *ch == '_' {
                function_name.push(chars_copy.next().unwrap());
            } else {
                break;
            }
        }
        match function_name.as_str() {
            "print" => {
                $chars = chars_copy;
                vec![Token::BuiltInFunction(BuiltInFunction::Print)]
            }
            "arg" => {
                $chars = chars_copy;
                vec![Token::BuiltInFunction(BuiltInFunction::Arg)]
            }
            "exec" => {
                $chars = chars_copy;
                vec![Token::BuiltInFunction(BuiltInFunction::Exec)]
            }
            "exit" => {
                $chars = chars_copy;
                vec![Token::BuiltInFunction(BuiltInFunction::Exit)]
            }
            _ => vec![],
        }
    }};
}

/// Matches reserved keywords.
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
            }
            "else" => {
                $chars = chars_copy;
                vec![Token::Keyword(Keyword::Else)]
            }
            "elseif" => {
                $chars = chars_copy;
                vec![Token::Keyword(Keyword::ElseIf)]
            }
            "while" => {
                $chars = chars_copy;
                vec![Token::Keyword(Keyword::While)]
            }
            "for" => {
                $chars = chars_copy;
                vec![Token::Keyword(Keyword::For)]
            }
            "let" => {
                $chars = chars_copy;
                vec![Token::Keyword(Keyword::Let)]
            }

            _ => vec![],
        }
    }};
}

/// Matches identifiers. Identifiers are sequences of characters that are used to name variables and functions.
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

/// Matches string literals. String literals are sequences of characters enclosed in double quotes.
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
            vec![Token::StringLiteral(string_val)]
        } else {
            vec![]
        }
    }};
}

/// Matches individual tokens based on their character patterns.
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
            },
            Some(&'<') => {
                $chars.next(); // Consume '<'
                match $chars.peek() {
                    Some(&'=') => {
                        $chars.next(); // Consume '='
                        vec![Token::Operator(Operator::LessThanOrEqual)]
                    }
                    _ => vec![Token::Operator(Operator::LessThan)],
                }
            },
            Some(&'>') => {
                $chars.next(); // Consume '>'
                match $chars.peek() {
                    Some(&'=') => {
                        $chars.next(); // Consume '='
                        vec![Token::Operator(Operator::GreaterThanOrEqual)]
                    }
                    _ => vec![Token::Operator(Operator::GreaterThan)],
                }
            },
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
                        tokens = match_builtin_function!($chars);
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
    /// Constructs a new `Lexer` with the provided input string.
    pub fn new(input: &str) -> Self {
        Lexer {
            input: input.to_string(),
        }
    }
    /// Tokenizes the input string and returns a vector of tokens.
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
                ('%', Some(Token::Operator(Operator::Modulo))),
                ('=', Some(Token::Assign)),
                ('(', Some(Token::OpenParen)),
                (')', Some(Token::CloseParen)),
                ('{', Some(Token::OpenBrace)),
                ('}', Some(Token::CloseBrace)),
                (',', Some(Token::Comma)),
                (';', Some(Token::EoL)),
                numbers,
                keywords,
                identifiers
            );

            if matched_tokens.is_empty() {
                chars.next(); // Skip unrecognized characters
            } else {
                tokens.extend(matched_tokens);
            }
        }

        tokens
    }
}

/// Unit tests for the lexer.
#[cfg(test)]
mod tests;
