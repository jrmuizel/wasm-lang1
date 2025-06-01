use std::iter::Peekable;
use std::str::Chars;

// Token definitions
#[derive(Debug, PartialEq, Clone)]
enum Token {
    Keyword(String),        // e.g., "int", "if"
    Identifier(String),     // e.g., "myVar"
    LiteralInt(i32),       // e.g., 42
    LiteralBool(bool),      // e.g., true/false
    LiteralString(String), // e.g., "hello"
    Operator(String),      // e.g., "+", "="
    Delimiter(char),       // e.g., '(', ';'
    EndOfInput,
}

// AST Node definitions
#[derive(Debug)]
enum Expr {
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
}

#[derive(Debug)]
enum Stmt {
    VariableDecl {
        var_type: String,
        name: String,
        init: Option<Expr>,
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
}

// Lexer implementation
struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Self {
        Lexer {
            chars: input.chars().peekable(),
        }
    }

    fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        
        match self.chars.peek() {
            Some(&c) => match c {
                '(' | ')' | '{' | '}' | ';' | ',' => {
                    self.chars.next();
                    Token::Delimiter(c)
                }
                '+' | '-' | '*' | '/' | '!' | '&' | '|' | '<' | '>' | '=' => {
                    let op = self.read_operator();
                    Token::Operator(op)
                }
                '"' => {
                    self.chars.next(); // Skip opening quote
                    let s = self.read_string();
                    Token::LiteralString(s)
                }
                _ if c.is_alphabetic() => {
                    let ident = self.read_identifier();
                    match ident.as_str() {
                        "int" | "boolean" | "String" | "if" | "else" | "while" | "print" | "true" | "false" => {
                            Token::Keyword(ident)
                        }
                        "true" => Token::LiteralBool(true),
                        "false" => Token::LiteralBool(false),
                        _ => Token::Identifier(ident),
                    }
                }
                _ if c.is_digit(10) => {
                    Token::LiteralInt(self.read_number())
                }
                _ => {
                    self.chars.next();
                    Token::Operator("?".to_string()) // Placeholder for unknown
                }
            },
            None => Token::EndOfInput,
        }
    }

    fn read_identifier(&mut self) -> String {
        let mut ident = String::new();
        while let Some(&c) = self.chars.peek() {
            if c.is_alphanumeric() || c == '_' {
                ident.push(c);
                self.chars.next();
            } else {
                break;
            }
        }
        ident
    }

    fn read_number(&mut self) -> i32 {
        let mut num_str = String::new();
        while let Some(&c) = self.chars.peek() {
            if c.is_digit(10) {
                num_str.push(c);
                self.chars.next();
            } else {
                break;
            }
        }
        num_str.parse().unwrap()
    }

    fn read_string(&mut self) -> String {
        let mut s = String::new();
        while let Some(&c) = self.chars.peek() {
            if c == '"' {
                self.chars.next(); // Skip closing quote
                break;
            }
            s.push(c);
            self.chars.next();
        }
        s
    }

    fn read_operator(&mut self) -> String {
        let mut op = String::new();
        op.push(self.chars.next().unwrap());
        if let Some(&next) = self.chars.peek() {
            if (op == "<" && next == '=') 
                || (op == ">" && next == '=')
                || (op == "!" && next == '=')
                || (op == "&" && next == '&')
                || (op == "|" && next == '|')
                || (op == "=" && next == '=')
            {
                op.push(next);
                self.chars.next();
            }
        }
        op
    }

    fn skip_whitespace(&mut self) {
        while let Some(&c) = self.chars.peek() {
            if c.is_whitespace() {
                self.chars.next();
            } else {
                break;
            }
        }
    }
}

// Parser implementation
struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, current: 0 }
    }

    fn parse(&mut self) -> Result<Vec<Stmt>, String> {
        let mut statements = Vec::new();
        while !self.is_at_end() {
            statements.push(self.statement()?);
        }
        Ok(statements)
    }

    fn statement(&mut self) -> Result<Stmt, String> {
        match self.peek() {
            Some(Token::Keyword(kw)) => match kw.as_str() {
                "int" | "boolean" | "String" => self.variable_decl(),
                "if" => self.if_statement(),
                "while" => self.while_statement(),
                "print" => self.print_statement(),
                _ => self.expression_statement(),
            },
            Some(Token::Delimiter('{')) => self.block(),
            _ => self.expression_statement(),
        }
    }

    fn variable_decl(&mut self) -> Result<Stmt, String> {
        let var_type = match self.advance() {
            Some(Token::Keyword(s)) => s,
            _ => return Err("Expected type keyword".to_string()),
        };

        let name = match self.advance() {
            Some(Token::Identifier(s)) => s,
            _ => return Err("Expected identifier".to_string()),
        };

        let init = if self.match_token(&Token::Operator("=".to_string())) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(&Token::Delimiter(';'), "Expected ';' after declaration")?;
        Ok(Stmt::VariableDecl { var_type, name, init })
    }

    fn if_statement(&mut self) -> Result<Stmt, String> {
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

    fn while_statement(&mut self) -> Result<Stmt, String> {
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

    fn print_statement(&mut self) -> Result<Stmt, String> {
        self.consume(&Token::Keyword("print".to_string()), "Expected 'print'")?;
        let expr = self.expression()?;
        self.consume(&Token::Delimiter(';'), "Expected ';' after print")?;
        Ok(Stmt::Print(expr))
    }

    fn block(&mut self) -> Result<Stmt, String> {
        self.consume(&Token::Delimiter('{'), "Expected '{'")?;
        let mut statements = Vec::new();
        while !self.check(&Token::Delimiter('}')) && !self.is_at_end() {
            statements.push(self.statement()?);
        }
        self.consume(&Token::Delimiter('}'), "Expected '}'")?;
        Ok(Stmt::Block(statements))
    }

    fn expression_statement(&mut self) -> Result<Stmt, String> {
        let expr = self.expression()?;
        self.consume(&Token::Delimiter(';'), "Expected ';' after expression")?;
        Ok(Stmt::Expression(expr))
    }

    fn expression(&mut self) -> Result<Expr, String> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr, String> {
        let expr = self.equality()?;
        if self.match_token(&Token::Operator("=".to_string())) {
            if let Expr::Variable(name) = expr {
                let value = self.assignment()?;
                return Ok(Expr::BinaryOp {
                    op: "=".to_string(),
                    left: Box::new(Expr::Variable(name)),
                    right: Box::new(value),
                });
            } else {
                return Err("Invalid assignment target".to_string());
            }
        }
        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr, String> {
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

    fn comparison(&mut self) -> Result<Expr, String> {
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

    fn term(&mut self) -> Result<Expr, String> {
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

    fn factor(&mut self) -> Result<Expr, String> {
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

    fn unary(&mut self) -> Result<Expr, String> {
        if let Some(op) = self.match_operator(&["!", "-"]) {
            let operand = self.unary()?;
            return Ok(Expr::UnaryOp {
                op: op.to_string(),
                operand: Box::new(operand),
            });
        }
        self.primary()
    }

    fn primary(&mut self) -> Result<Expr, String> {
        match self.advance() {
            Some(Token::LiteralInt(n)) => Ok(Expr::LiteralInt(n)),
            Some(Token::LiteralBool(b)) => Ok(Expr::LiteralBool(b)),
            Some(Token::LiteralString(s)) => Ok(Expr::LiteralString(s)),
            Some(Token::Identifier(name)) => Ok(Expr::Variable(name)),
            Some(Token::Delimiter('(')) => {
                let expr = self.expression()?;
                self.consume(&Token::Delimiter(')'), "Expected ')'")?;
                Ok(expr)
            }
            _ => Err("Expected expression".to_string()),
        }
    }

    // Helper methods
    fn advance(&mut self) -> Option<Token> {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.tokens.get(self.current - 1).cloned()
    }

    fn consume(&mut self, token: &Token, message: &str) -> Result<(), String> {
        if self.check(token) {
            self.advance();
            Ok(())
        } else {
            Err(message.to_string())
        }
    }

    fn check(&self, token: &Token) -> bool {
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

    fn peek(&self) -> Option<Token> {
        self.tokens.get(self.current).cloned()
    }

    fn is_at_end(&self) -> bool {
        matches!(self.peek(), Some(Token::EndOfInput) | None)
    }
}

fn main() {
    let input = r#"
        int x = 5;
        if (x > 0) {
            print("Positive");
        } else {
            print("Non-positive");
        }
    "#;

    // Tokenize input
    let mut lexer = Lexer::new(input);
    let mut tokens = Vec::new();
    loop {
        let token = lexer.next_token();
        if token == Token::EndOfInput {
            break;
        }
        tokens.push(token);
    }

    // Parse tokens
    let mut parser = Parser::new(tokens);
    match parser.parse() {
        Ok(ast) => println!("{:#?}", ast),
        Err(e) => eprintln!("Parse error: {}", e),
    }
}