mod parser;
mod codegen;

use parser::{Lexer, Parser};
use codegen::CodeGenerator;
use std::env;
use std::fs;
use std::process;
use std::path::Path;

use wasmtime::{Config, Engine, Linker, Module, Store, Caller, Rooted, ArrayRef};

fn main() {
    let args: Vec<String> = env::args().collect();
    
    let mut wasi_mode = false;
    let mut filename = String::new();
    
    // Parse command line arguments
    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            "--wasi" => {
                wasi_mode = true;
                i += 1;
            }
            arg if !arg.starts_with('-') => {
                filename = arg.to_string();
                i += 1;
            }
            _ => {
                eprintln!("Unknown argument: {}", args[i]);
                eprintln!("Usage: {} [--wasi] <source_file>", args[0]);
                process::exit(1);
            }
        }
    }
    
    if filename.is_empty() {
        eprintln!("Usage: {} [--wasi] <source_file>", args[0]);
        process::exit(1);
    }
    
    // Read the source file
    let input = match fs::read_to_string(&filename) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Error reading file '{}': {}", filename, e);
            process::exit(1);
        }
    };
    
    // Parse the program
    let lexer = Lexer::new(&input);
    let mut parser = Parser::new(lexer);
    let ast = match parser.parse() {
        Ok(ast) => ast,
        Err(e) => {
            eprintln!("Parse error: {}", e);
            process::exit(1);
        }
    };
    
    // Generate WASM
    let mut codegen = CodeGenerator::new(wasi_mode);
    let wat_code = codegen.generate(ast);
    
    if wasi_mode {
        // Output WAT file when using WASI mode
        let output_filename = filename.replace(".lang", ".wat");
        match fs::write(&output_filename, &wat_code) {
            Ok(()) => {
                println!("Generated WASI WAT file: {}", output_filename);
            }
            Err(e) => {
                eprintln!("Error writing WAT file '{}': {}", output_filename, e);
                process::exit(1);
            }
        }
        return;
    }
    
    // Convert WAT to WASM binary for non-WASI mode
    let wasm_binary = match wat::Parser::default().generate_dwarf(wat::GenerateDwarf::Lines).parse_str(Some(Path::new(&filename)), &wat_code) {
        Ok(binary) => binary,
        Err(e) => {
            eprintln!("WAT compilation error: {}", e);
            eprintln!("Generated WAT:\n{}", wat_code);
            process::exit(1);
        }
    };
    eprintln!("{}", wat_code);
    
    // Execute the WASM
    match execute_wasm(&wasm_binary) {
        Ok(()) => {},
        Err(e) => {
            eprintln!("Execution error: {}", e);
            process::exit(1);
        }
    }
}

fn execute_wasm(wasm_binary: &[u8]) -> Result<(), Box<dyn std::error::Error>> {
    // Setup wasmtime with GC support
    let mut config = Config::new();
    config.wasm_multi_memory(true);
    config.wasm_gc(true);
    let engine = Engine::new(&config)?;
    let module = Module::from_binary(&engine, wasm_binary)?;
    let mut linker = Linker::new(&engine);
    let mut store = Store::new(&engine, ());

    // Setup host functions for printing
    linker.func_wrap("host", "printInt", move |val: i32| {
        print!("{}", val);
    })?;

    linker.func_wrap("host", "printBool", move |val: i32| {
        print!("{}", if val != 0 { "true" } else { "false" });
    })?;

    linker.func_wrap("host", "printFloat", move |val: f32| {
        print!("{}", val);
    })?;

    linker.func_wrap("host", "printString", move |mut caller: Caller<'_, ()>, val: Rooted<ArrayRef>| {
        let len = val.len(&caller).unwrap_or(0);
        for i in 0..len {
            if let Ok(byte_val) = val.get(&mut caller, i) {
                if let Some(byte) = byte_val.i32() {
                    print!("{}", byte as u8 as char);
                }
            }
        }
    })?;

    // Setup host functions for println (printing with newlines)
    linker.func_wrap("host", "printlnInt", move |val: i32| {
        println!("{}", val);
    })?;

    linker.func_wrap("host", "printlnBool", move |val: i32| {
        println!("{}", if val != 0 { "true" } else { "false" });
    })?;

    linker.func_wrap("host", "printlnFloat", move |val: f32| {
        println!("{}", val);
    })?;

    linker.func_wrap("host", "printlnString", move |mut caller: Caller<'_, ()>, val: Rooted<ArrayRef>| {
        let len = val.len(&caller).unwrap_or(0);
        for i in 0..len {
            if let Ok(byte_val) = val.get(&mut caller, i) {
                if let Some(byte) = byte_val.i32() {
                    print!("{}", byte as u8 as char);
                }
            }
        }
        println!(); // Add newline after string
    })?;

    // Instantiate and run
    let instance = linker.instantiate(&mut store, &module)?;
    
    // Check if there's a main function and call it
    if let Some(main_func) = instance.get_func(&mut store, "main") {
        main_func.call(&mut store, &[], &mut [])?;
        println!(); // Add newline after execution
    } else {
        eprintln!("Warning: No main function found to execute");
    }

    Ok(())
}
