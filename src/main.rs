mod parser;
mod codegen;

use parser::{Lexer, Parser, Token};

fn main() {
    let input = r#"
        class MyClass {
            void myMethod(int param) {
                return param + 1;
                //return this.otherMethod(param);
            }
        }
        void main() {
            int x = 5;
            if (x > 0) {
                print(x);
            } else {
                print(x);
            }
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
        Ok(ast) => {
            println!("{:#?}", ast);
            let mut codegen = codegen::CodeGenerator::new();
            let wasm_code = codegen.generate(ast);
            println!("{}", wasm_code);
        }
        Err(e) => eprintln!("Parse error: {}", e),
    }
}
