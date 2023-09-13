use crate::lexer::{BuiltInFunction, Keyword, Operator, Token};

#[derive(Debug, Clone)]
pub enum ASTNode {
    Program(Vec<ASTNode>),
    Statement(Box<ASTNode>),
    BinaryOp {
        op: Operator,
        left: Box<ASTNode>,
        right: Box<ASTNode>,
    },
    Assign {
        name: String,
        value: Box<ASTNode>,
    },
    Variable(String),
    Number(i32),
    StringLiteral(String),
    If {
        condition: Box<ASTNode>,
        body: Box<ASTNode>,
        else_body: Option<Box<ASTNode>>,
    },
    While {
        condition: Box<ASTNode>,
        body: Box<ASTNode>,
    },
    BuiltInFunctionCall {
        name: BuiltInFunction,
        args: Vec<ASTNode>,
    },
    UserFunctionCall {
        name: String,
        args: Vec<ASTNode>,
    },
    // Add other control structures as needed
}

impl ASTNode {
    pub fn to_bash(&self) -> String {
        match self {
            ASTNode::Program(nodes) => nodes
                .iter()
                .map(|node| node.to_bash())
                .collect::<Vec<_>>()
                .join("\n"),
            ASTNode::Statement(node) => node.to_bash(),
            ASTNode::BinaryOp { op, left, right } => {
                format!(
                    "$(( {} {} {} ))",
                    left.to_bash(),
                    op.to_bash(),
                    right.to_bash()
                )
            }
            ASTNode::Assign { name, value } => {
                match **value {
                    ASTNode::BuiltInFunctionCall {
                        name: BuiltInFunction::Exec,
                        ref args,
                    } => {
                        // Special handling for `exec` when it's assigned
                        match &args[0] {
                            ASTNode::StringLiteral(s) => format!("{}=$({})", name, s),
                            ASTNode::Variable(var_name) => format!("{}=$({})", name, var_name),
                            _ => panic!("Invalid argument for Exec command!"), // or handle error accordingly
                        }
                    }
                    _ => format!("{}={}", name, value.to_bash()),
                }
            }
            ASTNode::Variable(name) => name.clone(),
            ASTNode::Number(n) => n.to_string(),
            ASTNode::If {
                condition,
                body,
                else_body,
            } => {
                let mut bash_code = format!(
                    "if (( {} ));\nthen\n{}\n",
                    condition.to_bash(),
                    body.to_bash()
                );
                if let Some(else_body) = else_body {
                    bash_code += &format!("else\n{}\n", else_body.to_bash());
                }
                bash_code + "fi"
            }
            ASTNode::While { condition, body } => {
                format!(
                    "while (( {} ));\ndo\n{}\ndone",
                    condition.to_bash(),
                    body.to_bash()
                )
            }
            ASTNode::BuiltInFunctionCall { name, args } => {
                match name {
                    BuiltInFunction::Exec => {
                        match &args[0] {
                            ASTNode::StringLiteral(s) => s.clone(),
                            ASTNode::Variable(var_name) => {
                                format!("${}", var_name)
                            }
                            _ => panic!("Invalid argument for Exec command!"), // or handle error accordingly
                        }
                    }
                    BuiltInFunction::Print => match &args[0] {
                        ASTNode::StringLiteral(s) => format!("echo {}", s),
                        ASTNode::Variable(name) => format!("echo ${}", name),
                        _ => format!("echo {}", args[0].to_bash()),
                    },
                    // ... (handle other functions as needed)
                    _ => format!(
                        "{} {}",
                        name.to_bash(),
                        args.iter()
                            .map(|arg| arg.to_bash())
                            .collect::<Vec<_>>()
                            .join(" ")
                    ),
                }
            }
            ASTNode::UserFunctionCall { name, args } => {
                let args_bash = args
                    .iter()
                    .map(|arg| arg.to_bash())
                    .collect::<Vec<_>>()
                    .join(" ");
                format!("{} {}", name, args_bash)
            }
            ASTNode::StringLiteral(s) => format!("\"{}\"", s),
            // Handle other cases as needed
        }
    }
}

impl BuiltInFunction {
    fn to_bash(&self) -> &'static str {
        match self {
            BuiltInFunction::Print => "echo",
            BuiltInFunction::Exec => "exec",
            BuiltInFunction::Arg => "arg",
            BuiltInFunction::Exit => "exit",
            // Add other mappings as needed
        }
    }
}

impl Operator {
    fn to_bash(&self) -> &'static str {
        match self {
            Operator::Plus => "+",
            Operator::Minus => "-",
            Operator::Multiply => "*",
            Operator::Divide => "/",
            Operator::Equal => "==",
            Operator::NotEqual => "!=",
            // Handle other operators as needed
        }
    }
}

macro_rules! parse_func_call {
    ($self:ident) => {{
        let current_token = $self.tokens[$self.position].clone(); // Cache the token
        $self.position += 1;
        let args = $self.parse_function_arguments()?;
        match current_token {
            Token::BuiltInFunction(func_name) => Ok(ASTNode::BuiltInFunctionCall {
                name: func_name,
                args,
            }),
            Token::Identifier(func_name) => Ok(ASTNode::UserFunctionCall {
                name: func_name,
                args,
            }),
            _ => Err(format!(
                "Expected a function name, found {:?}",
                current_token
            )),
        }
    }};
}

#[allow(unused_macros)]
macro_rules! parse_operator {
    ($self:ident) => {
        match &$self.tokens[$self.position] {
            Token::Operator(op) => {
                $self.position += 1;
                Ok(*op)
            }
            _ => Err("Expected an operator".to_string()),
        }
    };
}

macro_rules! parse_keyword {
    ($self:ident) => {
        match &$self.tokens[$self.position] {
            Token::Keyword(kw) => {
                $self.position += 1;
                Ok(kw)
            }
            _ => Err("Expected a keyword".to_string()),
        }
    };
}

macro_rules! parse_identifier {
    ($self:ident) => {
        match &$self.tokens[$self.position] {
            Token::Identifier(id) => {
                $self.position += 1;
                Ok(ASTNode::Variable(id.clone()))
            }
            _ => Err("Expected an identifier".to_string()),
        }
    };
}

#[allow(unused_macros)]
macro_rules! parse_number {
    ($self:ident) => {
        match &$self.tokens[$self.position] {
            Token::Number(n) => {
                $self.position += 1;
                Ok(ASTNode::Number(*n))
            }
            _ => Err("Expected a number".to_string()),
        }
    };
}

macro_rules! expect_token {
    ($self:ident, $token_variant:ident($val:ident) => $ast_node_variant:ident) => {
        match &$self.tokens[$self.position] { // Add reference here
            Token::$token_variant(ref $val) => { // Use ref keyword here
                $self.position += 1;
                Ok(ASTNode::$ast_node_variant($val.clone())) // Clone the value since it's a reference
            },
            _ => Err(format!("Expected a {}", stringify!($token_variant))),
        }
    };
    ($self:ident, $token_variant:ident => $ast_node_variant:ident) => {
        match $self.tokens[$self.position] {
            Token::$token_variant => {
                $self.position += 1;
                Ok(ASTNode::$ast_node_variant)
            },
            _ => Err(format!("Expected a {}", stringify!($token_variant))),
        }
    };
}

macro_rules! parse_variable_declaration {
    ($self:ident) => {
        if let Ok(Keyword::Let) = parse_keyword!($self) {
            if let Ok(ASTNode::Variable(name)) = parse_identifier!($self) {
                if let Some(Token::Assign) = $self.peek_next_token() {
                    $self.position += 1; // Consume Assign
                    let expr = $self.expression()?;
                    //$self.consume_optional_eol();

                    Ok(ASTNode::Assign {
                        name,
                        value: Box::new(expr),
                    })
                } else {
                    Ok(ASTNode::Variable(name))
                }
            } else {
                Err("Expected an identifier after 'let'".to_string())
            }
        } else {
            Err("Expected 'let' keyword".to_string())
        }
    };
}

macro_rules! parse_if_statement {
    ($self:ident) => {
        if let Ok(Keyword::If) = parse_keyword!($self) {
            let condition = $self.expression()?;
            if let Some(Token::OpenBrace) = $self.peek_next_token() {
                $self.position += 1; // Consume OpenBrace
                let body = $self.parse_block()?; // Assuming you have a method called parse_block
                if let Some(Token::Keyword(Keyword::Else)) = $self.peek_next_token() {
                    $self.position += 1; // Consume Else
                    if let Some(Token::OpenBrace) = $self.peek_next_token() {
                        $self.position += 1; // Consume OpenBrace
                        let else_body = $self.parse_block()?;
                        Ok(ASTNode::If {
                            condition: Box::new(condition),
                            body: Box::new(body),
                            else_body: Some(Box::new(else_body)),
                        })
                    } else {
                        Err("Expected '{' after 'else'".to_string())
                    }
                } else {
                    Ok(ASTNode::If {
                        condition: Box::new(condition),
                        body: Box::new(body),
                        else_body: None,
                    })
                }
            } else {
                Err("Expected '{' after if condition".to_string())
            }
        } else {
            Err("Expected 'if' keyword".to_string())
        }
    };
}

macro_rules! parse_while_loop {
    ($self:ident) => {
        if let Ok(Keyword::While) = parse_keyword!($self) {
            let condition = $self.expression()?; // Assuming you have a method called parse_expression
            if let Some(Token::OpenBrace) = $self.peek_next_token() {
                $self.position += 1; // Consume OpenBrace
                let body = $self.parse_block()?; // Assuming you have a method called parse_block
                Ok(ASTNode::While {
                    condition: Box::new(condition),
                    body: Box::new(body),
                })
            } else {
                Err("Expected '{' after while condition".to_string())
            }
        } else {
            Err("Expected 'while' keyword".to_string())
        }
    };
}

pub struct Parser {
    tokens: Vec<Token>,
    position: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens,
            position: 0,
        }
    }

    pub fn parse(&mut self) -> Result<ASTNode, String> {
        let mut statements = Vec::new();

        while self.position < self.tokens.len() {
            let stmt = self.statement()?;
            statements.push(stmt);

            // Here, simply consume the EoL token and move to the next statement
            if let Some(Token::EoL) = self.peek_next_token() {
                self.position += 1; // Consume EoL
            } else {
                return Err(format!("Expected EoL, found {:?}", self.peek_next_token()));
            }
        }
        Ok(ASTNode::Program(statements))
    }

    fn consume_optional_eol(&mut self) {
        if let Some(Token::EoL) = self.peek_next_token() {
            self.position += 1; // Consume EoL
        }
    }

    fn statement(&mut self) -> Result<ASTNode, String> {
        let stmt = match self.peek_next_token() {
            Some(Token::Keyword(Keyword::Let)) => parse_variable_declaration!(self),
            Some(Token::BuiltInFunction(_)) => parse_func_call!(self),
            Some(Token::Keyword(Keyword::If)) => parse_if_statement!(self),
            Some(Token::Keyword(Keyword::While)) => parse_while_loop!(self),
            Some(Token::Number(_)) => self.expression(),
            Some(Token::Identifier(_)) => {
                if let Some(Token::Assign) = self.peek_nth_token(1) {
                    self.expect_assignment()
                } else {
                    self.expression()
                }
            }
            _ => Err(format!(
                "Expected a statement, found {:?}",
                self.peek_next_token()
            )),
        }?;

        Ok(ASTNode::Statement(Box::new(stmt)))
    }

    fn parse_function_arguments(&mut self) -> Result<Vec<ASTNode>, String> {
        let mut args = Vec::new();

        // Expecting an open parenthesis after function name
        match &self.tokens[self.position] {
            Token::OpenParen => self.position += 1,
            _ => {
                return Err(format!(
                    "Expected '(' after function name, found {:?}",
                    &self.tokens[self.position]
                ))
            }
        }

        // Parse arguments until a close parenthesis is found
        while &self.tokens[self.position] != &Token::CloseParen {
            let expr = self.expression()?;
            args.push(expr);

            // If the next token is a comma, consume it and continue parsing the next argument
            if &self.tokens[self.position] == &Token::Comma {
                self.position += 1;
            }
        }

        // Consuming the close parenthesis
        self.position += 1;

        Ok(args)
    }

    fn parse_block(&mut self) -> Result<ASTNode, String> {
        let mut statements = Vec::new();

        loop {
            match self.peek_next_token() {
                Some(Token::CloseBrace) => {
                    self.position += 1;
                    break;
                }
                Some(Token::EoL) => {
                    self.position += 1; // consume the EoL
                }
                _ => {
                    let stmt = self.statement()?;
                    statements.push(stmt);
                    self.consume_optional_eol(); // Add this line
                }
            }
        }

        Ok(ASTNode::Program(statements))
    }

    fn expression(&mut self) -> Result<ASTNode, String> {
        let mut left = self.term()?;

        while let Some(op) = self.peek_next_operator(&[
            Operator::Plus,
            Operator::Minus,
            Operator::Equal,
            Operator::NotEqual,
        ]) {
            self.position += 1; // Consume Operator
            let right = self.term()?;
            left = ASTNode::BinaryOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn term(&mut self) -> Result<ASTNode, String> {
        let mut left = self.factor()?;

        while let Some(op) = self.peek_next_operator(&[Operator::Multiply, Operator::Divide]) {
            self.position += 1; // Consume Operator
            let right = self.factor()?;
            left = ASTNode::BinaryOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn factor(&mut self) -> Result<ASTNode, String> {
        if let Some(Token::Operator(Operator::Plus)) = self.peek_next_token() {
            self.position += 1; // Consume '+'
            return self.factor();
        }

        if let Some(Token::Operator(Operator::Minus)) = self.peek_next_token() {
            self.position += 1; // Consume '-'
            let right = self.primary()?;
            return Ok(ASTNode::BinaryOp {
                op: Operator::Minus,
                left: Box::new(ASTNode::Number(0)), // Represent as 0 - right
                right: Box::new(right),
            });
        }

        self.primary()
    }

    fn primary(&mut self) -> Result<ASTNode, String> {
        let next_token = self.peek_next_token().cloned();
        // First, handle expressions enclosed in parentheses
        if let Some(Token::OpenParen) = next_token {
            self.position += 1; // Consume '('
            let expr = self.expression()?; // Evaluate the enclosed expression
            if let Some(Token::CloseParen) = self.peek_next_token() {
                self.position += 1; // Consume ')'
                return Ok(expr);
            } else {
                return Err(format!("Expected ')', found {:?}", self.peek_next_token()));
            }
        }

        if let Some(Token::BuiltInFunction(_)) = next_token {
            return parse_func_call!(self);
        }

        if let Some(Token::StringLiteral(s)) = next_token {
            self.position += 1;
            return Ok(ASTNode::StringLiteral(s));
        }

        // Next, handle numbers and variables
        match next_token {
            Some(Token::Number(_)) => self.expect_number(),
            Some(Token::Identifier(_)) => {
                if let Some(Token::Assign) = self.peek_nth_token(1) {
                    self.expect_assignment()
                } else {
                    self.expect_variable()
                }
            }
            // Here you can extend to handle other primary expressions
            _ => Err(format!(
                "Expected a primary expression, found {:?}",
                self.peek_next_token()
            )),
        }
    }

    fn peek_next_token(&self) -> Option<&Token> {
        if self.position < self.tokens.len() {
            Some(&self.tokens[self.position])
        } else {
            None
        }
    }

    fn peek_next_operator(&self, ops: &[Operator]) -> Option<Operator> {
        if self.position >= self.tokens.len() {
            return None;
        }

        match self.tokens[self.position] {
            Token::Operator(ref op) if ops.contains(op) => Some(op.clone()),
            _ => None,
        }
    }

    fn peek_nth_token(&self, n: usize) -> Option<&Token> {
        if self.position + n < self.tokens.len() {
            Some(&self.tokens[self.position + n])
        } else {
            None
        }
    }

    fn expect_assignment(&mut self) -> Result<ASTNode, String> {
        let name = match &self.tokens[self.position] {
            Token::Identifier(ref ident) => ident.clone(),
            _ => {
                return Err(format!(
                    "Expected an identifier, found {:?}",
                    self.tokens[self.position]
                ))
            }
        };
        self.position += 2; // Consume Identifier and Assign
        let value = self.expression()?;
        Ok(ASTNode::Assign {
            name,
            value: Box::new(value),
        })
    }

    fn expect_variable(&mut self) -> Result<ASTNode, String> {
        expect_token!(self, Identifier(name) => Variable)
    }

    fn expect_number(&mut self) -> Result<ASTNode, String> {
        expect_token!(self, Number(val) => Number)
    }
}

#[cfg(test)]
mod tests;
