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
    let mut browser_mode = false;
    let mut filename = String::new();
    
    // Parse command line arguments
    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            "--wasi" => {
                wasi_mode = true;
                i += 1;
            }
            "--browser" => {
                browser_mode = true;
                i += 1;
            }
            arg if !arg.starts_with('-') => {
                filename = arg.to_string();
                i += 1;
            }
            _ => {
                eprintln!("Unknown argument: {}", args[i]);
                eprintln!("Usage: {} [--wasi|--browser] <source_file>", args[0]);
                process::exit(1);
            }
        }
    }
    
    if filename.is_empty() {
        eprintln!("Usage: {} [--wasi|--browser] <source_file>", args[0]);
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
    let target_mode = if wasi_mode {
        codegen::TargetMode::Wasi
    } else if browser_mode {
        codegen::TargetMode::Browser
    } else {
        codegen::TargetMode::Host
    };
    
    let mut codegen = CodeGenerator::new(target_mode);
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
    
    if browser_mode {
        // Generate browser-compatible files
        generate_browser_files(&filename, &wat_code);
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

fn generate_browser_files(filename: &str, wat_code: &str) {
    // Generate .wasm file
    let wasm_binary = match wat::Parser::default().generate_dwarf(wat::GenerateDwarf::Lines).parse_str(Some(Path::new(&filename)), &wat_code) {
        Ok(binary) => binary,
        Err(e) => {
            eprintln!("WAT compilation error: {}", e);
            eprintln!("Generated WAT:\n{}", wat_code);
            process::exit(1);
        }
    };
    
    let wasm_filename = filename.replace(".lang", ".wasm");
    match fs::write(&wasm_filename, &wasm_binary) {
        Ok(()) => {
            println!("Generated WASM file: {}", wasm_filename);
        }
        Err(e) => {
            eprintln!("Error writing WASM file '{}': {}", wasm_filename, e);
            process::exit(1);
        }
    }
    
    // Generate HTML file
    let html_filename = filename.replace(".lang", ".html");
    let html_content = generate_html_template(&wasm_filename);
    match fs::write(&html_filename, &html_content) {
        Ok(()) => {
            println!("Generated HTML file: {}", html_filename);
            println!("Open {} in a web browser to run the program", html_filename);
        }
        Err(e) => {
            eprintln!("Error writing HTML file '{}': {}", html_filename, e);
            process::exit(1);
        }
    }
}

fn generate_html_template(wasm_filename: &str) -> String {
    format!(r#"<!DOCTYPE html>
<html>
<head>
    <title>Lang1 WebAssembly Runner</title>
    <style>
        body {{
            font-family: Arial, sans-serif;
            max-width: 800px;
            margin: 50px auto;
            padding: 20px;
        }}
        #output {{
            background: #f5f5f5;
            padding: 15px;
            border-radius: 5px;
            font-family: 'Courier New', monospace;
            white-space: pre-wrap;
            min-height: 100px;
            border: 1px solid #ddd;
        }}
        button {{
            background: #007bff;
            color: white;
            border: none;
            padding: 10px 20px;
            border-radius: 5px;
            cursor: pointer;
            margin: 10px 5px 10px 0;
        }}
        button:hover {{
            background: #0056b3;
        }}
        .error {{
            color: red;
            font-weight: bold;
        }}
    </style>
</head>
<body>
    <h1>Lang1 WebAssembly Runner</h1>
    <p>This page runs your compiled Lang1 program in WebAssembly.</p>
    
    <button onclick="runProgram()">Run Program</button>
    <button onclick="clearOutput()">Clear Output</button>
    
    <h2>Output:</h2>
    <div id="output"></div>

    <script>
        let wasmInstance = null;
        let wasmMemory = null;
        
        async function loadWasm() {{
            try {{
                const wasmModule = await WebAssembly.instantiateStreaming(fetch('{wasm_filename}'));
                wasmInstance = wasmModule.instance;
                wasmMemory = wasmInstance.exports.memory;
                
                // Clear output buffer
                wasmInstance.exports.clearOutput();
                
                console.log('WebAssembly module loaded successfully');
                return true;
            }} catch (error) {{
                console.error('Error loading WebAssembly:', error);
                document.getElementById('output').innerHTML = '<span class="error">Error loading WebAssembly: ' + error.message + '</span>';
                return false;
            }}
        }}
        
        function getOutput() {{
            if (!wasmInstance || !wasmMemory) return '';
            
            const outputPtr = wasmInstance.exports.getOutputPtr();
            const outputLen = wasmInstance.exports.getOutputLen();
            
            if (outputLen === 0) return '';
            
            const buffer = new Uint8Array(wasmMemory.buffer, outputPtr, outputLen);
            return new TextDecoder().decode(buffer);
        }}
        
        async function runProgram() {{
            const outputElement = document.getElementById('output');
            
            if (!wasmInstance) {{
                const loaded = await loadWasm();
                if (!loaded) return;
            }}
            
            try {{
                // Clear previous output
                wasmInstance.exports.clearOutput();
                
                // Run the main function
                wasmInstance.exports.main();
                
                // Get the output and display it
                const output = getOutput();
                outputElement.textContent = output || '(no output)';
                
            }} catch (error) {{
                console.error('Error running program:', error);
                outputElement.innerHTML = '<span class="error">Runtime error: ' + error.message + '</span>';
            }}
        }}
        
        function clearOutput() {{
            document.getElementById('output').textContent = '';
            if (wasmInstance) {{
                wasmInstance.exports.clearOutput();
            }}
        }}
        
        // Load WebAssembly on page load
        window.addEventListener('load', loadWasm);
    </script>
</body>
</html>"#, wasm_filename = wasm_filename)
}
