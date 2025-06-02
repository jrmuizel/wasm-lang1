mod parser;
mod codegen;

use parser::{Lexer, Parser, Token};

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
