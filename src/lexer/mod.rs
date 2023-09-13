// Module: lexer
// Purpose: lexing the input
// Path: src/lexer/mod.rs

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

#[derive(PartialEq, Debug, Clone)]
pub enum Keyword {
    If,
    Else,
    ElseIf,
    While,
    For,
    Let,
}

#[derive(PartialEq, Debug, Clone)]
pub enum BuiltInFunction {
    Print,
    Arg,
    Exec,
    Exit,
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
            vec![Token::StringLiteral(string_val)]
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

#[cfg(test)]
mod tests;
