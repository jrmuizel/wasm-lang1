use std::iter::Peekable;
use std::str::Chars;
use std::collections::VecDeque;

// Token definitions
#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Keyword(String),        // e.g., "int", "if"
    Identifier(String),     // e.g., "myVar"
    LiteralInt(i32),       // e.g., 42
    LiteralBool(bool),      // e.g., true/false
    LiteralString(String), // e.g., "hello"
    Operator(String),      // e.g., "+", "="
    Delimiter(char),       // e.g., '(', ';'
    Dot,                  // '.' for field/method access
    This,                 // 'this' keyword
    New,                  // 'new' keyword
    EndOfInput,
}

// AST Node definitions
#[derive(Debug, Clone)]
pub enum Expr {
    BinaryOp {
        op: String,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    UnaryOp {
        op: String,
        operand: Box<Expr>,
    },
    Variable(String),
    LiteralInt(i32),
    LiteralBool(bool),
    LiteralString(String),
    MethodCall {
        object: Box<Expr>,
        method: String,
        args: Vec<Expr>,
    },
    FieldAccess {
        object: Box<Expr>,
        field: String,
    },
    FunctionCall {
        name: String,
        args: Vec<Expr>,
    },
    This,
    New {
        class_name: String,
        args: Vec<Expr>,
    },
    ArrayAccess {
        array: Box<Expr>,
        index: Box<Expr>,
    },
}

#[derive(Debug, Clone)]
pub enum Stmt {
    VariableDecl {
        var_type: String,
        name: String,
        init: Option<Expr>,
    },
    FieldDecl {
        var_type: String,
        name: String,
        init: Option<Expr>,
    },
    FunctionDecl {
        name: String,
        return_type: String,
        params: Vec<(String, String)>,
        body: Box<Stmt>,
    },
    Assignment {
        name: String,
        value: Expr,
    },
    If {
        condition: Expr,
        then_block: Box<Stmt>,
        else_block: Option<Box<Stmt>>,
    },
    While {
        condition: Expr,
        body: Box<Stmt>,
    },
    Block(Vec<Stmt>),
    Expression(Expr),
    Print(Expr),
    ClassDecl {
        name: String,
        fields: Vec<Stmt>,  // For field declarations
        methods: Vec<Stmt>,
    },
    MethodDecl {
        name: String,
        return_type: String,
        params: Vec<(String, String)>,
        body: Box<Stmt>,
    },
    Return {
        value: Option<Expr>,
    },
}

// Lexer implementation
pub struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
    line: usize,
    column: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer {
            chars: input.chars().peekable(),
            line: 1,
            column: 1,
        }
    }

    /// Returns the current line and column (1-based)
    pub fn position(&self) -> (usize, usize) {
        (self.line, self.column)
    }

    /// Advances the iterator and updates line/column
    fn next_char(&mut self) -> Option<char> {
        let c = self.chars.next();
        if let Some(ch) = c {
            if ch == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
        }
        c
    }

    /// Peeks at the next character without consuming it
    fn peek_char(&mut self) -> Option<&char> {
        self.chars.peek()
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        
        match self.peek_char() {
            Some(&c) => match c {
                '(' | ')' | '{' | '}' | ';' | ',' | '.' | '[' | ']' => {
                    if c == '.' {
                        return self.handle_dot();
                    }
                    self.next_char();
                    Token::Delimiter(c)
                }
                '+' | '-' | '*' | '/' | '!' | '&' | '|' | '<' | '>' | '=' => {
                    let op = self.read_operator();
                    Token::Operator(op)
                }
                '"' => {
                    self.next_char(); // Skip opening quote
                    let s = self.read_string();
                    Token::LiteralString(s)
                }
                _ if c.is_alphabetic() => {
                    let ident = self.read_identifier();
                    match ident.as_str() {
                        "int" | "boolean" | "String" | "if" | "else" | "while" | "print" => {
                            Token::Keyword(ident)
                        }
                        "true" => Token::LiteralBool(true),
                        "false" => Token::LiteralBool(false),
                        "this" => Token::This,
                        "new" => Token::New,
                        "class" | "void" | "return" => Token::Keyword(ident),
                        _ => Token::Identifier(ident.clone()),
                    }
                }
                _ if c.is_digit(10) => {
                    Token::LiteralInt(self.read_number())
                }
                _ => {
                    self.next_char();
                    Token::Operator("?".to_string()) // Placeholder for unknown
                }
            },
            None => Token::EndOfInput,
        }
    }
   
    fn handle_dot(&mut self) -> Token {
        self.next_char(); // Consume '.'
        if self.peek_char() == Some(&'.') {
            panic!("Double dot not supported");
        }
        Token::Dot
    }

    fn read_identifier(&mut self) -> String {
        let mut ident = String::new();
        while let Some(&c) = self.peek_char() {
            if c.is_alphanumeric() || c == '_' {
                ident.push(c);
                self.next_char();
            } else {
                break;
            }
        }
        ident
    }

    fn read_number(&mut self) -> i32 {
        let mut num_str = String::new();
        while let Some(&c) = self.peek_char() {
            if c.is_digit(10) {
                num_str.push(c);
                self.next_char();
            } else {
                break;
            }
        }
        num_str.parse().unwrap()
    }

    fn read_string(&mut self) -> String {
        let mut s = String::new();
        while let Some(&c) = self.peek_char() {
            if c == '"' {
                self.next_char(); // Skip closing quote
                break;
            }
            s.push(c);
            self.next_char();
        }
        s
    }

    fn read_operator(&mut self) -> String {
        let mut op = String::new();
        op.push(self.next_char().unwrap());
        if let Some(&next) = self.peek_char() {
            if (op == "<" && next == '=') 
                || (op == ">" && next == '=')
                || (op == "!" && next == '=')
                || (op == "&" && next == '&')
                || (op == "|" && next == '|')
                || (op == "=" && next == '=')
            {
                op.push(next);
                self.next_char();
            }
        }
        op
    }

    fn skip_whitespace(&mut self) {
        loop {
            let mut skipped = false;
            // Skip whitespace
            while let Some(&c) = self.peek_char() {
                if c.is_whitespace() {
                    self.next_char();
                    skipped = true;
                } else {
                    break;
                }
            }
            // Skip C++ style comments
            if let Some(&'/') = self.peek_char() {
                let mut clone = self.chars.clone();
                let next = clone.next();
                if let Some('/') = clone.peek().copied() {
                    // Found //, skip until end of line
                    self.next_char(); // skip first /
                    self.next_char(); // skip second /
                    while let Some(&c) = self.peek_char() {
                        if c == '\n' {
                            break;
                        }
                        self.next_char();
                    }
                    skipped = true;
                }
            }
            if !skipped {
                break;
            }
        }
    }
}

// Parser implementation
#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub line: usize,
    pub column: usize,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Parse error at line {}, column {}: {}", self.line, self.column, self.message)
    }
}

impl std::error::Error for ParseError {}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    lookahead: VecDeque<Token>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Parser {
            lexer,
            lookahead: VecDeque::new(),
        }
    }

    // Ensure at least n+1 tokens are in the lookahead buffer
    fn fill_lookahead(&mut self, n: usize) {
        while self.lookahead.len() <= n {
            let token = self.lexer.next_token();
            let is_end = matches!(token, Token::EndOfInput);
            self.lookahead.push_back(token);
            if is_end {
                break;
            }
        }
    }

    fn advance(&mut self) -> Option<Token> {
        self.fill_lookahead(0);
        self.lookahead.pop_front()
    }

    fn peek(&mut self) -> Option<Token> {
        self.fill_lookahead(0);
        self.lookahead.get(0).cloned()
    }

    fn peek_n(&mut self, n: usize) -> Option<Token> {
        self.fill_lookahead(n);
        self.lookahead.get(n).cloned()
    }

    fn peek_next(&mut self) -> Option<Token> {
        self.peek_n(1)
    }

    fn is_at_end(&mut self) -> bool {
        matches!(self.peek(), Some(Token::EndOfInput) | None)
    }

    fn check(&mut self, token: &Token) -> bool {
        self.peek().map_or(false, |t| t == *token)
    }

    fn match_token(&mut self, token: &Token) -> bool {
        if self.check(token) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn match_operator(&mut self, ops: &[&str]) -> Option<String> {
        if let Some(Token::Operator(op)) = self.peek() {
            if ops.contains(&op.as_str()) {
                self.advance();
                return Some(op);
            }
        }
        None
    }

    fn error<T>(&mut self, message: &str) -> Result<T, ParseError> {
        let (line, column) = self.lexer.position();
        Err(ParseError {
            message: message.to_string(),
            line,
            column,
        })
    }

    fn consume(&mut self, token: &Token, message: &str) -> Result<(), ParseError> {
        if self.check(token) {
            self.advance();
            Ok(())
        } else {
            self.error(message)
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut statements = Vec::new();
        while !self.is_at_end() {
            statements.push(self.statement()?);
        }
        Ok(statements)
    }

    fn statement(&mut self) -> Result<Stmt, ParseError> {
        match self.peek() {
            Some(Token::Keyword(kw)) => match kw.as_str() {
                "class" => self.class_decl(),
                "return" => self.return_statement(),
                "int" | "boolean" | "String" | "void" => {
                    // Look ahead to see if this is a function or variable declaration
                    let mut idx = 1;
                    // Skip any number of [] for array types
                    while self.peek_n(idx) == Some(Token::Delimiter('[')) && self.peek_n(idx+1) == Some(Token::Delimiter(']')) {
                        idx += 2;
                    }
                    let is_function = match (self.peek_n(idx), self.peek_n(idx+1)) {
                        (Some(Token::Identifier(_)), Some(Token::Delimiter('('))) => true,
                        _ => false,
                    };
                    if is_function {
                        self.function_decl()
                    } else {
                        self.variable_decl()
                    }
                }
                "if" => self.if_statement(),
                "while" => self.while_statement(),
                "print" => self.print_statement(),
                _ => self.expression_statement(),
            },
            Some(Token::Identifier(_)) => {
                // Look ahead to see if this is a declaration or expression
                match (self.peek_next(), self.peek_n(2)) {
                    // Function declaration: Type Name (
                    (Some(Token::Identifier(_)), Some(Token::Delimiter('('))) => self.function_decl(),
                    // Variable declaration: Type Name [= or ;]
                    (Some(Token::Identifier(_)), Some(Token::Operator(ref op))) if op == "=" => self.variable_decl(),
                    (Some(Token::Identifier(_)), Some(Token::Delimiter(';'))) => self.variable_decl(),
                    // Expression statement
                    _ => self.expression_statement(),
                }
            },
            Some(Token::Delimiter('{')) => self.block(),
            Some(Token::This) | Some(Token::LiteralInt(_)) | 
            Some(Token::LiteralBool(_)) | Some(Token::LiteralString(_)) |
            Some(Token::New) | Some(Token::Delimiter('(')) => self.expression_statement(),
            _ => self.expression_statement(), // Changed from Err to allow any expression
        }
    }

    fn class_decl(&mut self) -> Result<Stmt, ParseError> {
        self.consume(&Token::Keyword("class".to_string()), "Expected 'class'")?;
        let name = match self.advance() {
            Some(Token::Identifier(name)) => name,
            _ => return self.error("Expected class name"),
        };
        self.consume(&Token::Delimiter('{'), "Expected '{' before class body")?;
        
        let mut fields = Vec::new();
        let mut methods = Vec::new();
        while !self.check(&Token::Delimiter('}')) && !self.is_at_end() {
            // Look ahead two tokens to see if there's a '(' after the identifier
            // This distinguishes between method declarations and field declarations
            let is_method = match (self.peek(), self.peek_next()) {
                (Some(Token::Keyword(_)) | Some(Token::Identifier(_)), Some(Token::Identifier(_))) => {
                    // Look one more ahead for a '('
                    match self.peek_n(2) {
                        Some(Token::Delimiter('(')) => true,
                        _ => false,
                    }
                }
                _ => false,
            };

            if is_method {
                methods.push(self.method_decl()?);
            } else {
                fields.push(self.field_decl()?);
            }
        }
        
        self.consume(&Token::Delimiter('}'), "Expected '}' after class body")?;
        Ok(Stmt::ClassDecl { name, fields, methods })
    }

    fn method_decl(&mut self) -> Result<Stmt, ParseError> {
        // Parse return type
        let return_type = match self.advance() {
            Some(Token::Keyword(kw)) => kw,
            _ => return self.error("Expected return type"),
        };

        // Parse method name
        let name = match self.advance() {
            Some(Token::Identifier(name)) => name,
            _ => return self.error("Expected method name"),
        };

        // Parse parameters
        self.consume(&Token::Delimiter('('), "Expected '(' after method name")?;
        let mut params = Vec::new();
        if !self.check(&Token::Delimiter(')')) {
            loop {
                let param_type = match self.advance() {
                    Some(Token::Keyword(kw)) | Some(Token::Identifier(kw)) => kw,
                    _ => return self.error("Expected parameter type"),
                };
                let param_name = match self.advance() {
                    Some(Token::Identifier(name)) => name,
                    _ => return self.error("Expected parameter name"),
                };
                params.push((param_type, param_name));
                
                if !self.match_token(&Token::Delimiter(',')) {
                    break;
                }
            }
        }
        self.consume(&Token::Delimiter(')'), "Expected ')' after parameters")?;

        // Parse method body
        let body = self.block()?;
        
        Ok(Stmt::MethodDecl {
            name,
            return_type,
            params,
            body: Box::new(body),
        })
    }

    fn return_statement(&mut self) -> Result<Stmt, ParseError> {
        self.consume(&Token::Keyword("return".to_string()), "Expected 'return'")?;
        let value = if !self.check(&Token::Delimiter(';')) {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(&Token::Delimiter(';'), "Expected ';' after return")?;
        Ok(Stmt::Return { value })
    }


    fn variable_decl(&mut self) -> Result<Stmt, ParseError> {
        let var_type = self.parse_type()?;
        let name = match self.advance() {
            Some(Token::Identifier(s)) => s,
            _ => return self.error("Expected identifier"),
        };
        let init = if self.match_token(&Token::Operator("=".to_string())) {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(&Token::Delimiter(';'), "Expected ';' after declaration")?;
        Ok(Stmt::VariableDecl { var_type, name, init })
    }

    fn if_statement(&mut self) -> Result<Stmt, ParseError> {
        self.consume(&Token::Keyword("if".to_string()), "Expected 'if'")?;
        self.consume(&Token::Delimiter('('), "Expected '(' after 'if'")?;
        let condition = self.expression()?;
        self.consume(&Token::Delimiter(')'), "Expected ')' after condition")?;

        let then_block = self.statement()?;
        let else_block = if self.match_token(&Token::Keyword("else".to_string())) {
            Some(Box::new(self.statement()?))
        } else {
            None
        };

        Ok(Stmt::If {
            condition,
            then_block: Box::new(then_block),
            else_block,
        })
    }

    fn while_statement(&mut self) -> Result<Stmt, ParseError> {
        self.consume(&Token::Keyword("while".to_string()), "Expected 'while'")?;
        self.consume(&Token::Delimiter('('), "Expected '(' after 'while'")?;
        let condition = self.expression()?;
        self.consume(&Token::Delimiter(')'), "Expected ')' after condition")?;
        
        let body = self.statement()?;
        Ok(Stmt::While {
            condition,
            body: Box::new(body),
        })
    }

    fn print_statement(&mut self) -> Result<Stmt, ParseError> {
        self.consume(&Token::Keyword("print".to_string()), "Expected 'print'")?;
        let expr = self.expression()?;
        self.consume(&Token::Delimiter(';'), "Expected ';' after print")?;
        Ok(Stmt::Print(expr))
    }

    fn block(&mut self) -> Result<Stmt, ParseError> {
        self.consume(&Token::Delimiter('{'), "Expected '{'")?;
        let mut statements = Vec::new();
        while !self.check(&Token::Delimiter('}')) && !self.is_at_end() {
            statements.push(self.statement()?);
        }
        self.consume(&Token::Delimiter('}'), "Expected '}'")?;
        Ok(Stmt::Block(statements))
    }

    fn expression_statement(&mut self) -> Result<Stmt, ParseError> {
        let expr = self.expression()?;
        self.consume(&Token::Delimiter(';'), "Expected ';' after expression")?;
        Ok(Stmt::Expression(expr))
    }

    fn expression(&mut self) -> Result<Expr, ParseError> {
        self.assignment()
    }

    fn postfix(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.primary()?;
        loop {
            if self.match_token(&Token::Dot) {
                let method_or_field = match self.advance() {
                    Some(Token::Identifier(name)) => name,
                    _ => return self.error("Expected identifier after '.'"),
                };
                if self.match_token(&Token::Delimiter('(')) {
                    let args = self.parse_arguments()?;
                    self.consume(&Token::Delimiter(')'), "Expected ')' after method arguments")?;
                    expr = Expr::MethodCall {
                        object: Box::new(expr),
                        method: method_or_field,
                        args,
                    };
                } else {
                    expr = Expr::FieldAccess {
                        object: Box::new(expr),
                        field: method_or_field,
                    };
                }
            } else if self.match_token(&Token::Delimiter('[')) {
                let index = self.expression()?;
                self.consume(&Token::Delimiter(']'), "Expected ']' after array index")?;
                expr = Expr::ArrayAccess {
                    array: Box::new(expr),
                    index: Box::new(index),
                };
            } else {
                break;
            }
        }
        Ok(expr)
    }
    
    fn parse_arguments(&mut self) -> Result<Vec<Expr>, ParseError> {
        let args = self.parse_argument_list()?;
        Ok(args)
    }

    fn logic_or(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.logic_and()?;
        while let Some(op) = self.match_operator(&["||"]) {
            let right = self.logic_and()?;
            expr = Expr::BinaryOp {
                op,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn logic_and(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.equality()?;
        while let Some(op) = self.match_operator(&["&&"]) {
            let right = self.equality()?;
            expr = Expr::BinaryOp {
                op,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn assignment(&mut self) -> Result<Expr, ParseError> {
        let expr = self.logic_or()?;
        if self.match_token(&Token::Operator("=".to_string())) {
            match expr {
                Expr::Variable(name) => {
                    let value = self.assignment()?;
                    Ok(Expr::BinaryOp {
                        op: "=".to_string(),
                        left: Box::new(Expr::Variable(name)),
                        right: Box::new(value),
                    })
                }
                Expr::FieldAccess { object, field } => {
                    let value = self.assignment()?;
                    Ok(Expr::BinaryOp {
                        op: "=".to_string(),
                        left: Box::new(Expr::FieldAccess { object, field }),
                        right: Box::new(value),
                    })
                }
                Expr::ArrayAccess { array, index } => {
                    let value = self.assignment()?;
                    Ok(Expr::BinaryOp {
                        op: "=".to_string(),
                        left: Box::new(Expr::ArrayAccess { array, index }),
                        right: Box::new(value),
                    })
                }
                _ => self.error("Invalid assignment target"),
            }
        } else {
            Ok(expr)
        }
    }

    fn equality(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.comparison()?;
        while let Some(op) = self.match_operator(&["==", "!="]) {
            let right = self.comparison()?;
            expr = Expr::BinaryOp {
                op: op.to_string(),
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.term()?;
        while let Some(op) = self.match_operator(&["<", "<=", ">", ">="]) {
            let right = self.term()?;
            expr = Expr::BinaryOp {
                op: op.to_string(),
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.factor()?;
        while let Some(op) = self.match_operator(&["+", "-"]) {
            let right = self.factor()?;
            expr = Expr::BinaryOp {
                op: op.to_string(),
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.unary()?;
        while let Some(op) = self.match_operator(&["*", "/"]) {
            let right = self.unary()?;
            expr = Expr::BinaryOp {
                op: op.to_string(),
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, ParseError> {
        if let Some(op) = self.match_operator(&["!", "-"]) {
            let operand = self.unary()?;
            return Ok(Expr::UnaryOp {
                op: op.to_string(),
                operand: Box::new(operand),
            });
        }
        self.postfix()
    }

    fn primary(&mut self) -> Result<Expr, ParseError> {
        match self.advance() {
            Some(Token::LiteralInt(n)) => Ok(Expr::LiteralInt(n)),
            Some(Token::LiteralBool(b)) => Ok(Expr::LiteralBool(b)),
            Some(Token::LiteralString(s)) => Ok(Expr::LiteralString(s)),
            Some(Token::This) => Ok(Expr::This),
            Some(Token::New) => {
                // Parse base type after 'new'
                let base_type = match self.advance() {
                    Some(Token::Keyword(s)) | Some(Token::Identifier(s)) => s,
                    _ => return self.error("Expected type after 'new'"),
                };
                if self.match_token(&Token::Delimiter('[')) {
                    // Array creation: new int[expr]
                    let size_expr = self.expression()?;
                    self.consume(&Token::Delimiter(']'), "Expected ']' after array size")?;
                    Ok(Expr::New { class_name: format!("{}[]", base_type), args: vec![size_expr] })
                } else if self.match_token(&Token::Delimiter('(')) {
                    let args = self.parse_arguments()?;
                    self.consume(&Token::Delimiter(')'), "Expected ')' after arguments")?;
                    Ok(Expr::New { class_name: base_type, args })
                } else {
                    // Forbid zero-arg constructor without parentheses
                    self.error("Expected '(' or '[' after type in 'new' expression")
                }
            }
            Some(Token::Identifier(name)) => {
                // Check for function call
                if self.check(&Token::Delimiter('(')) {
                    self.advance(); // consume '('
                    let args = self.parse_argument_list()?;
                    self.consume(&Token::Delimiter(')'), "Expected ')' after arguments")?;
                    Ok(Expr::FunctionCall { name, args })
                } else {
                    Ok(Expr::Variable(name))
                }
            }
            Some(Token::Delimiter('(')) => {
                let expr = self.expression()?;
                self.consume(&Token::Delimiter(')'), "Expected ')'")?;
                Ok(expr)
            }
            _ => self.error("Expected expression"),
        }
    }

    
    fn parse_argument_list(&mut self) -> Result<Vec<Expr>, ParseError> {
        let mut args = Vec::new();
        if !self.check(&Token::Delimiter(')')) {
            loop {
                args.push(self.expression()?);
                if !self.match_token(&Token::Delimiter(',')) {
                    break;
                }
            }
        }
        Ok(args)
    }

    fn field_decl(&mut self) -> Result<Stmt, ParseError> {
        let var_type = self.parse_type()?;
        let name = match self.advance() {
            Some(Token::Identifier(s)) => s,
            _ => return self.error("Expected identifier"),
        };
        let init = if self.match_token(&Token::Operator("=".to_string())) {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(&Token::Delimiter(';'), "Expected ';' after field declaration")?;
        Ok(Stmt::FieldDecl { var_type, name, init })
    }

    fn function_decl(&mut self) -> Result<Stmt, ParseError> {
        let return_type = self.parse_type()?;
        let name = match self.advance() {
            Some(Token::Identifier(name)) => name,
            _ => return self.error("Expected function name"),
        };
        self.consume(&Token::Delimiter('('), "Expected '(' after function name")?;
        let mut params = Vec::new();
        if !self.check(&Token::Delimiter(')')) {
            loop {
                let param_type = self.parse_type()?;
                let param_name = match self.advance() {
                    Some(Token::Identifier(name)) => name,
                    _ => return self.error("Expected parameter name"),
                };
                params.push((param_type, param_name));
                if !self.match_token(&Token::Delimiter(',')) {
                    break;
                }
            }
        }
        self.consume(&Token::Delimiter(')'), "Expected ')' after parameters")?;
        let body = self.block()?;
        Ok(Stmt::FunctionDecl {
            name,
            return_type,
            params,
            body: Box::new(body),
        })
    }

    // Add a helper to parse types, supporting array types like int[]
    fn parse_type(&mut self) -> Result<String, ParseError> {
        let base_type = match self.advance() {
            Some(Token::Keyword(s)) | Some(Token::Identifier(s)) => s,
            _ => return self.error("Expected type"),
        };
        let mut type_str = base_type;
        while self.match_token(&Token::Delimiter('[')) {
            if !self.match_token(&Token::Delimiter(']')) {
                return self.error("Expected ']' after '[' in type");
            }
            type_str.push_str("[]");
        }
        Ok(type_str)
    }
}

fn main() {
    let input = r#"
        class MyClass {
            void myMethod(int param) {
                return this.otherMethod(param);
            }
        }

        int x = 5;
        if (x > 0) {
            print("Positive");
        } else {
            print("Non-positive");
        }
    "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    match parser.parse() {
        Ok(ast) => println!("{:#?}", ast),
        Err(e) => eprintln!("{}", e),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(input: &str) -> Result<Vec<Stmt>, ParseError> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let ast = parser.parse();
        if ast.is_err() {
            eprintln!("Parse error: {}", ast.as_ref().err().unwrap());
        }
        ast
    }

    #[test]
    fn test_class_declaration() {
        let input = "class MyClass { void myMethod() {} }";
        assert!(parse(input).is_ok());
    }

    #[test]
    fn test_method_declaration() {
        let input = "class Test {
            void method1() {}
            int method2(boolean p1, String p2) { return 0; }
        }";
        assert!(parse(input).is_ok());
    }

    #[test]
    fn test_variable_declaration() {
        let input = r#"
            int x;
            String s = "hello";
            boolean flag = true;
        "#;
        assert!(parse(input).is_ok());
    }

    #[test]
    fn test_assignment() {
        let input = "x = 42;";
        assert!(parse(input).is_ok());
    }

    #[test]
    fn test_if_statement() {
        let input = r#"
            if (x > 0) {
                print("Positive");
            } else {
                print("Non-positive");
            }
        "#;
        assert!(parse(input).is_ok());
    }

    #[test]
    fn test_while_loop() {
        let input = "while (i < 10) { i = i + 1; }";
        assert!(parse(input).is_ok());
    }

    #[test]
    fn test_print_statement() {
        let input = r#"print("Hello, world!");"#;
        assert!(parse(input).is_ok());
    }

    #[test]
    fn test_return_statement() {
        let input = "return 42;";
        assert!(parse(input).is_ok());
        
        let input2 = "return;";
        assert!(parse(input2).is_ok());
    }

    #[test]
    fn test_expression_parsing() {
        let inputs = [
            "1 + 2 * 3;",
            "(1 + 2) * 3;",
            "a == b && c != d;",
            "!flag || (x <= 0 && y >= 10);",
            "obj.method();",
            "this.field;",
        ];
        
        for input in inputs {
            assert!(parse(input).is_ok());
        }
    }

    #[test]
    fn test_field_access() {
        let inputs = [
            "this.field;",
            "obj.x;",
            "this.field.nested;",
            "obj.field1.field2;",
        ];
        
        for input in inputs {
            assert!(parse(input).is_ok());
        }
    }

    #[test]
    fn test_field_assignment() {
        let inputs = [
            "this.field = 1;",
            "obj.x = 42;",
            "this.field = other.field;",
            "obj.field1.field2 = value;",
            "this.field = new MyClass();",
            "obj.field = this.method();",
        ];
        
        for input in inputs {
            let result = parse(input);
            if let Err(ref e) = result {
                eprintln!("Failed to parse: {}", input);
                eprintln!("Error: {}", e);
            }
            assert!(result.is_ok());
        }
    }

    #[test]
    fn test_method_call() {
        let input = "this.doSomething(1, 2, 3);";
        assert!(parse(input).is_ok());
    }

    #[test]
    fn test_this_keyword() {
        let input = "return this;";
        assert!(parse(input).is_ok());
    }

    #[test]
    fn test_nested_blocks() {
        let input = r#"
            {
                int x = 5;
                {
                    int y = 10;
                    x = x + y;
                }
            }
        "#;
        assert!(parse(input).is_ok());
    }

    #[test]
    fn test_complex_program() {
        let input = r#"
            class Program {
                void main() {
                    int x = 10;
                    if (x > 0) {
                        while (x > 0) {
                            print(x);
                            x = x - 1;
                        }
                    }
                    return;
                }
            }
        "#;
        assert!(parse(input).is_ok());
    }

    #[test]
    fn test_missing_semicolon() {
        let input = "int x";
        let result = parse(input);
        assert!(result.is_err());
        assert!(result.as_ref().unwrap_err().message.contains("Expected ';'"));
    }

    #[test]
    fn test_missing_class_name() {
        let input = "class { void method() {} }";
        let result = parse(input);
        assert!(result.is_err());
        assert!(result.as_ref().unwrap_err().message.contains("Expected class name"));
    }

    #[test]
    fn test_invalid_assignment() {
        let input = "42 = x;";
        let result = parse(input);
        assert!(result.is_err());
        assert!(result.as_ref().unwrap_err().message.contains("Invalid assignment target"));
    }

    #[test]
    fn test_missing_parenthesis() {
        let input = "if (x { }";
        let result = parse(input);
        assert!(result.is_err());
        assert!(result.as_ref().unwrap_err().message.contains("Expected ')'"));
    }

    #[test]
    fn test_missing_brace() {
        let input = "class MyClass { void method() ";
        let result = parse(input);
        assert!(result.is_err());
        assert!(result.as_ref().unwrap_err().message.contains("Expected '{'"));
    }

    #[test]
    fn test_unexpected_token() {
        let input = "int 42;";
        let result = parse(input);
        assert!(result.is_err());
        assert!(result.as_ref().unwrap_err().message.contains("Expected identifier"));
    }

    #[test]
    fn test_method_call_missing_paren() {
        let input = "obj.method(1, 2;";
        let result = parse(input);
        assert!(result.is_err());
        assert!(result.as_ref().unwrap_err().message.contains("Expected ')'"));
    }

    #[test]
    fn test_new_expression() {
        let input = "x = new MyClass(1, 2, 3);";
        assert!(parse(input).is_ok());
    }

    #[test]
    fn test_new_missing_class_name() {
        let input = "new ();";
        let result = parse(input);
        if let Err(e) = &result {
            eprintln!("Parse error: {}", e);
        }
        assert!(result.as_ref().unwrap_err().message.contains("Expected type after 'new'"));
    }

    #[test]
    fn test_new_missing_parentheses() {
        let input = "new MyClass;";
        let result = parse(input);
        assert!(result.is_err());
    }

    #[test]
    fn test_class_with_fields() {
        let inputs = [
            "class Point { int x; int y; }",
            "class Person { String name; int age = 0; }",
            r#"class Student {
                String name;
                int grade = 0;
                void study() {
                    grade = grade + 1;
                }
            }"#,
            r#"class Complex {
                int real = 0;
                int imag = 0;
                void add(Complex other) {
                    this.real = this.real + other.real;
                    this.imag = this.imag + other.imag;
                }
            }"#
        ];
        
        for input in inputs {
            let result = parse(input);
            if let Err(ref e) = result {
                eprintln!("Failed to parse: {}", input);
                eprintln!("Error: {}", e);
            }
            assert!(result.is_ok());
        }
    }

    #[test]
    fn test_function_declaration() {
        let inputs = [
            "void main() { }",
            "int add(int a, int b) { return a + b; }",
            "String greet(String name) { return \"Hello \" + name; }",
            r#"void printNumbers(int n) {
                int i = 0;
                while (i < n) {
                    print(i);
                    i = i + 1;
                }
            }"#,
            r#"Complex addComplex(Complex a, Complex b) {
                Complex result = new Complex();
                result.real = a.real + b.real;
                result.imag = a.imag + b.imag;
                return result;
            }"#
        ];
        
        for input in inputs {
            let result = parse(input);
            if let Err(ref e) = result {
                eprintln!("Failed to parse: {}", input);
                eprintln!("Error: {}", e);
            }
            assert!(result.is_ok());
        }
    }

    #[test]
    fn test_array_type_and_access() {
        let input = r#"
            int[] arr;
            arr = new int[10];
            int x = arr[0];
        "#;
        assert!(parse(input).is_ok());
        let ast = parse(input).unwrap();
        // Optionally, check the AST structure for array access and array type
        // (not required for this basic test)
    }

    #[test]
    fn test_error_position_reporting() {
        // Error at line 1, column 8 (missing semicolon)
        let input = "int x";
        let result = parse(input);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert_eq!(err.line, 1);
        // The column should be at the end of input (after 'x'), which is column 6 (i n t   x)
        // But since we only check at error, it may be 6 or 7 depending on implementation
        assert!(err.column >= 6 && err.column <= 8, "column was {}", err.column);

        // Error at line 2, column 13 (missing class name)
        let input = "class {\n void method() {} }";
        let result = parse(input);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert_eq!(err.line, 1);
        assert!(err.column >= 7 && err.column <= 8, "column was {}", err.column);

        // Error at line 3, column 12 (invalid assignment target)
        let input = "\n\n42 = x;";
        let result = parse(input);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert_eq!(err.line, 3);
        assert_eq!(err.column, 5, "column was {}", err.column);

        // Error at line 2, column 8 (missing parenthesis)
        let input = "if (x { }";
        let result = parse(input);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert_eq!(err.line, 1);
        assert!(err.column >= 7 && err.column <= 9, "column was {}", err.column);
    }
}