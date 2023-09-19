// Module: parser
// Purpose: parsing the tokens into an AST
// Path: src/parser/mod.rs

use crate::lexer::{BuiltInFunction, Keyword, Operator, Token};

use std::collections::{HashMap, HashSet};

#[derive(Default, Debug)]
pub struct SymbolTable {
    table: HashMap<String, i32>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            table: HashMap::new(),
        }
    }

    pub fn set(&mut self, name: &str, value: i32) {
        self.table.insert(name.to_string(), value);
    }

    pub fn get(&self, name: &str) -> Option<i32> {
        self.table.get(name).cloned()
    }

    pub fn contains(&self, name: &str) -> bool {
        self.table.contains_key(name)
    }
}

#[derive(PartialEq, Debug, Clone)]
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
    Conditional {
        branches: Vec<(Box<ASTNode>, Box<ASTNode>)>, // (condition, body)
        else_body: Option<Box<ASTNode>>,
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
            ASTNode::Assign { name, value } => match **value {
                ASTNode::BuiltInFunctionCall {
                    name: BuiltInFunction::Exec,
                    ref args,
                } => match &args[0] {
                    ASTNode::StringLiteral(s) => format!("{}=$({})", name, s),
                    ASTNode::Variable(var_name) => format!("{}=$({})", name, var_name),
                    _ => panic!("Invalid argument for Exec command!"),
                },
                _ => format!("{}={}", name, value.to_bash()),
            },
            ASTNode::Variable(name) => name.clone(),
            ASTNode::Number(n) => n.to_string(),
            ASTNode::While { condition, body } => {
                format!(
                    "while (( {} ));\ndo\n{}\ndone",
                    condition.to_bash(),
                    body.to_bash()
                )
            }
            ASTNode::BuiltInFunctionCall { name, args } => {
                match name {
                    BuiltInFunction::Exec => match &args[0] {
                        ASTNode::StringLiteral(s) => s.clone(),
                        ASTNode::Variable(var_name) => {
                            format!("${}", var_name)
                        }
                        _ => panic!("Invalid argument for Exec command!"),
                    },
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
            ASTNode::Conditional {
                branches,
                else_body,
            } => {
                let mut bash_code = String::new();

                for (index, (condition, body)) in branches.iter().enumerate() {
                    match index {
                        0 => {
                            bash_code += &format!(
                                "if (( {} )); then\n{}\n",
                                condition.to_bash(),
                                body.to_bash()
                            )
                        } // Handle `if`
                        _ => {
                            bash_code += &format!(
                                "elif (( {} )); then\n{}\n",
                                condition.to_bash(),
                                body.to_bash()
                            )
                        } // Handle `elif`
                    }
                }

                if let Some(else_node) = else_body {
                    bash_code += &format!("else\n{}\n", else_node.to_bash());
                }

                bash_code + "fi"
            } // Handle other cases as needed
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
            Operator::Modulo => "%",
            Operator::Equal => "==",
            Operator::NotEqual => "!=",
            Operator::GreaterThan => ">",
            Operator::LessThan => "<",
            Operator::GreaterThanOrEqual => ">=",
            Operator::LessThanOrEqual => "<=",
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
        match &$self.tokens[$self.position] {
            Token::$token_variant(ref $val) => {
                $self.position += 1;
                Ok(ASTNode::$ast_node_variant($val.clone()))
            }
            _ => Err(format!("Expected a {}", stringify!($token_variant))),
        }
    };
    ($self:ident, $token_variant:ident => $ast_node_variant:ident) => {
        match $self.tokens[$self.position] {
            Token::$token_variant => {
                $self.position += 1;
                Ok(ASTNode::$ast_node_variant)
            }
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

                    // Check if expr is a constant and update the symbol table
                    match &expr {
                        ASTNode::Number(n) => $self.symtable.set(&name, *n),
                        _ => {
                            if $self.symtable.contains(&name) {
                                $self.symtable.table.remove(&name);
                            }
                        }
                    }

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
            let mut branches = Vec::new();
            let condition = $self.expression()?;
            if let Some(Token::OpenBrace) = $self.peek_next_token() {
                $self.position += 1; // Consume OpenBrace
                let body = $self.parse_block()?;
                branches.push((Box::new(condition), Box::new(body)));

                let mut else_body: Option<Box<ASTNode>> = None;

                loop {
                    match $self.peek_next_token() {
                        Some(Token::Keyword(Keyword::ElseIf)) => {
                            $self.position += 1; // Consume ElseIf
                            let else_if_condition = $self.expression()?;
                            if let Some(Token::OpenBrace) = $self.peek_next_token() {
                                $self.position += 1; // Consume OpenBrace
                                let else_if_body = $self.parse_block()?;
                                branches
                                    .push((Box::new(else_if_condition), Box::new(else_if_body)));
                            } else {
                                return Err("Expected '{' after else if condition".to_string());
                            }
                        }
                        Some(Token::Keyword(Keyword::Else)) => {
                            $self.position += 1; // Consume Else
                            if let Some(Token::OpenBrace) = $self.peek_next_token() {
                                $self.position += 1; // Consume OpenBrace
                                let else_block = $self.parse_block()?;
                                else_body = Some(Box::new(else_block));
                                break;
                            } else {
                                return Err("Expected '{' after 'else'".to_string());
                            }
                        }
                        _ => break,
                    }
                }

                Ok(ASTNode::Conditional {
                    branches,
                    else_body,
                })
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
            let condition = $self.expression()?;
            if let Some(Token::OpenBrace) = $self.peek_next_token() {
                $self.position += 1; // Consume OpenBrace
                let body = $self.parse_block()?;
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

fn remove_unused_variables(ast: ASTNode) -> ASTNode {
    // Step 1: Collect used variables
    let mut used_vars = HashSet::new();
    collect_used_variables(&ast, &mut used_vars);

    dbg!(&used_vars);
    // Step 2: Filter out unused assignments
    let tmp = filter_unused_assignments(ast, &used_vars).unwrap();

    dbg!(&tmp);
    tmp
}

fn collect_used_variables(node: &ASTNode, used_vars: &mut HashSet<String>) {
    match node {
        ASTNode::Variable(name) => {
            used_vars.insert(name.clone());
        }
        ASTNode::Program(statements) => {
            for stmt in statements {
                collect_used_variables(stmt, used_vars);
            }
        }
        ASTNode::BinaryOp { left, right, .. } => {
            collect_used_variables(left, used_vars);
            collect_used_variables(right, used_vars);
        }
        ASTNode::Assign { value, .. } => {
            collect_used_variables(value, used_vars);
        }
        ASTNode::BuiltInFunctionCall { args, .. } | ASTNode::UserFunctionCall { args, .. } => {
            for arg in args {
                collect_used_variables(arg, used_vars);
            }
        }
        ASTNode::Statement(inner) => {
            match &**inner {
                ASTNode::Assign {value, .. } => {
                    collect_used_variables(value, used_vars);
                }
                _ => collect_used_variables(&*inner, used_vars),
            }
        }
        // Handle other node types as necessary
        _ => {}
    }
}

fn filter_unused_assignments(node: ASTNode, used_vars: &HashSet<String>) -> Option<ASTNode> {
    dbg!("Entering filter_unused_assignments", &node);

    let result = match node {
        ASTNode::Program(statements) => {
            let filtered_statements: Vec<ASTNode> = statements
                .into_iter()
                .filter_map(|stmt| {
                    if let ASTNode::Assign { name, .. } = &stmt {
                        if !used_vars.contains(name) {
                            dbg!("Filtering out unused assignment:", name);
                            return None; // Filter out unused assignments
                        }
                        return filter_unused_assignments(stmt, used_vars);
                    }
                    Some(stmt)
                })
                .collect();
            Some(ASTNode::Program(filtered_statements))
        }
        ASTNode::Statement(inner) => {
            if let Some(filtered_inner) = filter_unused_assignments(*inner, used_vars) {
                Some(ASTNode::Statement(Box::new(filtered_inner)))
            } else {
                None
            }
        }
        // ... handle other cases
        _ => Some(node),
    };

    dbg!("Exiting filter_unused_assignments", &result);
    result
}

fn is_variable_used_in_ast(node: &ASTNode, var_name: &str) -> bool {
    match node {
        ASTNode::Variable(name) => name == var_name,
        ASTNode::Program(statements) => statements
            .iter()
            .any(|stmt| is_variable_used_in_ast(stmt, var_name)),
        ASTNode::BinaryOp { left, right, .. } => {
            is_variable_used_in_ast(left, var_name) || is_variable_used_in_ast(right, var_name)
        }
        ASTNode::Assign { value, .. } => is_variable_used_in_ast(value, var_name),
        ASTNode::BuiltInFunctionCall { args, .. } | ASTNode::UserFunctionCall { args, .. } => args
            .iter()
            .any(|arg| is_variable_used_in_ast(arg, var_name)),
        ASTNode::Statement(inner) => is_variable_used_in_ast(&*inner, var_name),
        // Handle other node types as necessary
        _ => false,
    }
}

fn propagate_constants(node: ASTNode, symtable: &mut SymbolTable) -> ASTNode {
    match node {
        ASTNode::Program(statements) => {
            let optimized_statements = statements
                .into_iter()
                .map(|stmt| propagate_constants(stmt, symtable))
                .collect();
            ASTNode::Program(optimized_statements)
        }
        ASTNode::Statement(inner) => {
            ASTNode::Statement(Box::new(propagate_constants(*inner, symtable)))
        }
        ASTNode::Assign { name, value } => {
            let new_value = propagate_constants(*value, symtable);

            // If the new_value is a number, update the symbol table
            if let ASTNode::Number(val) = &new_value {
                symtable.set(&name, *val);
            }

            ASTNode::Assign {
                name,
                value: Box::new(new_value),
            }
        }
        ASTNode::BuiltInFunctionCall { name, args } => {
            let optimized_args = args
                .into_iter()
                .map(|arg| propagate_constants(arg, symtable))
                .collect();
            ASTNode::BuiltInFunctionCall {
                name,
                args: optimized_args,
            }
        }
        ASTNode::Variable(name) => {
            if let Some(value) = symtable.get(&name) {
                ASTNode::Number(value)
            } else {
                ASTNode::Variable(name)
            }
        }
        ASTNode::BinaryOp { op, left, right } => ASTNode::BinaryOp {
            op,
            left: Box::new(propagate_constants(*left, symtable)),
            right: Box::new(propagate_constants(*right, symtable)),
        },
        _ => node,
    }
}

fn fold_constants(node: ASTNode) -> ASTNode {
    match node {
        ASTNode::Program(statements) => {
            let folded_statements = statements.into_iter().map(fold_constants).collect();
            ASTNode::Program(folded_statements)
        }
        ASTNode::Statement(inner) => ASTNode::Statement(Box::new(fold_constants(*inner))),
        ASTNode::Assign { name, value } => ASTNode::Assign {
            name,
            value: Box::new(fold_constants(*value)),
        },
        ASTNode::BuiltInFunctionCall { name, args } => {
            let folded_args = args.into_iter().map(fold_constants).collect();
            ASTNode::BuiltInFunctionCall {
                name,
                args: folded_args,
            }
        }
        ASTNode::BinaryOp { op, left, right } => {
            let left = fold_constants(*left);
            let right = fold_constants(*right);
            if let (ASTNode::Number(l_val), ASTNode::Number(r_val)) = (&left, &right) {
                let result = match op {
                    Operator::Plus => l_val + r_val,
                    Operator::Minus => l_val - r_val,
                    Operator::Multiply => l_val * r_val,
                    Operator::Divide => l_val / r_val,
                    Operator::Modulo => l_val % r_val,
                    _ => {
                        return ASTNode::BinaryOp {
                            op,
                            left: Box::new(left),
                            right: Box::new(right),
                        }
                    }
                };
                ASTNode::Number(result)
            } else {
                ASTNode::BinaryOp {
                    op,
                    left: Box::new(left),
                    right: Box::new(right),
                }
            }
        }
        _ => node,
    }
}

pub struct Parser {
    tokens: Vec<Token>,
    position: usize,
    symtable: SymbolTable,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens,
            position: 0,
            symtable: SymbolTable::new(),
        }
    }

    pub fn parse(&mut self) -> Result<ASTNode, String> {
        let mut statements = Vec::new();

        while self.position < self.tokens.len() {
            let stmt = self.statement()?;
            statements.push(stmt);

            if let Some(Token::EoL) = self.peek_next_token() {
                self.position += 1; // Consume EoL
            } else {
                return Err(format!("Expected EoL, found {:?}", self.peek_next_token()));
            }
        }
        let mut ast = ASTNode::Program(statements);
        let mut previous_ast = ast.clone();

        loop {
            let optimized_ast = remove_unused_variables(self.optimize(ast.clone()));
            if optimized_ast == previous_ast {
                break;
            }
            previous_ast = ast;
            ast = optimized_ast;
        }

        Ok(ast)
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
        while self.tokens[self.position] != Token::CloseParen {
            let expr = self.expression()?;
            args.push(expr);

            // If the next token is a comma, consume it and continue parsing the next argument
            if self.tokens[self.position] == Token::Comma {
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
                    self.consume_optional_eol();
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
            //Operator::Modulo,
            Operator::Equal,
            Operator::NotEqual,
            Operator::GreaterThan,
            Operator::LessThan,
            Operator::GreaterThanOrEqual,
            Operator::LessThanOrEqual,
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

        while let Some(op) =
            self.peek_next_operator(&[Operator::Multiply, Operator::Divide, Operator::Modulo])
        {
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

    pub fn optimize(&mut self, node: ASTNode) -> ASTNode {
        let node = propagate_constants(node, &mut self.symtable);
        fold_constants(node)
    }
}

#[cfg(test)]
mod tests;
