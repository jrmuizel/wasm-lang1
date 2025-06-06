mod parser;
mod codegen;

use parser::{Lexer, Parser, Token};
use std::fs::File;
use std::io::Write;

fn main() {
    let input = r#"
        class MyClass {
            void myMethod(int param) {
                return param + 1;
                //return this.otherMethod(param);
            }
        }

        void foo() {
            printInt(1);
        }

        void main() {
            int x = 5;
            if (x > 0) {
                printInt(x);
            } else {
                printInt(x);
            }
            
            for (int i = 0; i < 3; i = i + 1) {
                printInt(i);
            }
        }
    "#;

    let lexer = parser::Lexer::new(input);
    let mut parser = parser::Parser::new(lexer);
    match parser.parse() {
        Ok(ast) => {
            println!("{:#?}", ast);
            let mut codegen = codegen::CodeGenerator::new();
            let wasm_code = codegen.generate(ast);
            println!("{}", wasm_code);
            // write to file
            let mut file = File::create("output.wat").unwrap();
            file.write_all(wasm_code.as_bytes()).unwrap();
            let parser = wat::Parser::new();
            parser.parse_str(None, wasm_code ).unwrap();
        }
        Err(e) => eprintln!("Parse error: {}", e),
    }
}
