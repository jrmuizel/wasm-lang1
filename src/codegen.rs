// codegen.rs

use crate::parser::{Stmt, Expr, Token};
use std::collections::{HashMap, HashSet};
use std::cell::RefCell;
use wasmtime::{Caller, Val, FuncType, ValType, ArrayRef, Rooted};

pub struct CodeGenerator {
    classes: HashMap<String, Vec<(String, String)>>,
    output: RefCell<String>,
    indent_level: usize,
    local_vars_stack: Vec<HashMap<String, String>>, // Stack of local variable names to types
    current_class: Option<String>,
}

impl CodeGenerator {
    pub fn new() -> Self {
        CodeGenerator {
            classes: HashMap::new(),
            output: RefCell::new(String::new()),
            indent_level: 0,
            local_vars_stack: Vec::new(),
            current_class: None,
        }
    }

    pub fn generate(&mut self, stmts: Vec<Stmt>) -> String {
        self.output.borrow_mut().push_str("(module\n");
        self.indent_level += 1;

        // Import print functions
        self.emit_line("(import \"host\" \"printInt\" (func $printInt (param i32)))");
        self.emit_line("(import \"host\" \"printBool\" (func $printBool (param i32)))");
        self.emit_line("(import \"host\" \"printString\" (func $printString (param (ref array))))");
        self.emit_line("(import \"host\" \"printFloat\" (func $printFloat (param f32)))");

        // Define String type
        self.emit_line("(type $String (array i8))");

        // First pass: collect class information
        for stmt in &stmts {
            if let Stmt::ClassDecl { name, fields, .. } = stmt {
                let mut class_fields = Vec::new();
                for field in fields {
                    if let Stmt::FieldDecl { var_type, name: field_name, .. } = field {
                        class_fields.push((field_name.clone(), var_type.clone()));
                    }
                }
                self.classes.insert(name.clone(), class_fields);
            }
        }

        // Define class types
        for (class_name, fields) in &self.classes {
            self.emit(&format!("(type ${} (struct", class_name));
            for (field_name, field_type) in fields {
                let wasm_type = self.type_to_wasm(field_type);
                self.emit(&format!("\n  (field ${} (mut {}))", field_name, wasm_type));
            }
            self.emit("))\n");
        }

        // Generate functions and methods
        for stmt in &stmts {
            match stmt {
                Stmt::FunctionDecl { name, return_type, params, body } => {
                    self.generate_function(&name, &return_type, &params, &body, false);
                }
                Stmt::ClassDecl { name, methods, .. } => {
                    for method in methods {
                        if let Stmt::MethodDecl { name: method_name, return_type, params, body } = method {
                            self.generate_method(&name, &method_name, &return_type, &params, &body);
                        }
                    }
                }
                Stmt::VariableDecl { name, var_type, init, .. } => {
                    let wasm_type = self.type_to_wasm(&var_type);
                    self.emit(&format!("(global ${} (mut {})", name, wasm_type));
                    if let Some(expr) = init {
                        let value = self.generate_expr(&expr);
                        self.emit(&format!("(global.set ${} ({}))\n", name, value));
                    }
                }
                _ => {}
            }
        }

        // Generate main function if present
        if let Some(main) = self.find_main(&stmts) {
            self.emit_line("(export \"main\" (func $main))");
        }

        self.indent_level -= 1;
        self.output.borrow_mut().push_str(")");
        self.output.borrow().clone()
    }

    fn generate_function(
        &mut self,
        name: &str,
        return_type: &str,
        params: &[(String, String)],
        body: &Stmt,
        is_main: bool,
    ) {
        self.emit(&format!("(func ${}", name));
        let mut locals_map = std::collections::HashMap::new();
        for (param_type, param_name) in params.iter() {
            self.emit(&format!(
                " (param ${} {})",
                param_name,
                self.type_to_wasm(param_type)
            ));
            locals_map.insert(param_name.clone(), param_type.clone());
        }
        let result_type = self.type_to_wasm(return_type);
        if !result_type.is_empty() {
            self.emit(&format!(" (result {})\n", result_type));
        } else {
            self.emit("\n");
        }
        // Emit local variable declarations
        let locals = self.collect_locals(body, &locals_map.keys().cloned().collect());
        for (name, var_type) in &locals {
            self.emit(&format!(" (local ${} {})\n", name, self.type_to_wasm(var_type)));
            locals_map.insert(name.clone(), var_type.clone());
        }
        self.indent_level += 1;
        self.local_vars_stack.push(locals_map);
        self.generate_stmt(body);
        if return_type == "void" {
            self.emit_line("return");
        }
        self.local_vars_stack.pop();
        self.indent_level -= 1;
        self.emit_line(")");
    }

    fn generate_method(
        &mut self,
        class_name: &str,
        method_name: &str,
        return_type: &str,
        params: &[(String, String)],
        body: &Stmt,
    ) {
        let prev_class = self.current_class.clone();
        self.current_class = Some(class_name.to_string());
        self.emit(&format!("(func ${}.{}", class_name, method_name));
        self.emit(&format!(" (param $this (ref ${}))", class_name));
        let mut locals_map = std::collections::HashMap::new();
        locals_map.insert("this".to_string(), class_name.to_string());
        for (param_type, param_name) in params.iter() {
            self.emit(&format!(
                " (param ${} {})",
                param_name,
                self.type_to_wasm(param_type)
            ));
            locals_map.insert(param_name.clone(), param_type.clone());
        }
        let result_type = self.type_to_wasm(return_type);
        if !result_type.is_empty() {
            self.emit(&format!(" (result {})\n", result_type));
        } else {
            self.emit("\n");
        }
        // Emit local variable declarations
        let locals = self.collect_locals(body, &locals_map.keys().cloned().collect());
        for (name, var_type) in &locals {
            self.emit(&format!(" (local ${} {})\n", name, self.type_to_wasm(var_type)));
            locals_map.insert(name.clone(), var_type.clone());
        }
        self.indent_level += 1;
        self.local_vars_stack.push(locals_map);
        self.generate_stmt(body);
        if return_type == "void" {
            self.emit_line("return");
        }
        self.local_vars_stack.pop();
        self.indent_level -= 1;
        self.emit_line(")");
        // Export the method so it can be called
        self.emit_line(&format!("(export \"{}.{}\" (func ${}.{}))", class_name, method_name, class_name, method_name));
        self.current_class = prev_class;
    }

    fn generate_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Block(stmts) => {
                // New scope for block
                self.local_vars_stack.push(HashMap::new());
                for s in stmts {
                    // If it's a variable declaration, add to locals
                    if let Stmt::VariableDecl { name, var_type, .. } = s {
                        if let Some(locals) = self.local_vars_stack.last_mut() {
                            locals.insert(name.clone(), var_type.clone());
                        }
                    }
                    self.generate_stmt(s);
                }
                self.local_vars_stack.pop();
            }
            Stmt::VariableDecl { name, init, .. } => {
                // Already added to locals in Block
                if let Some(expr) = init {
                    let value = self.generate_expr(expr);
                    self.emit(&format!("(local.set ${} ({}))\n", name, value));
                }
            }
            Stmt::Assignment { name, value } => {
                let value = self.generate_expr(value);
                self.emit(&format!("(global.set ${} ({}))\n", name, value));
            }
            Stmt::If { condition, then_block, else_block } => {
                let cond = self.generate_expr(condition);
                self.emit(&format!("(if ({})\n", cond));
                self.indent_level += 1;
                self.emit_line("(then");
                self.generate_stmt(then_block);
                self.emit_line(")");
                if let Some(else_block) = else_block {
                    self.emit_line("(else");
                    self.generate_stmt(else_block);
                    self.emit_line(")");
                }
                self.indent_level -= 1;
                self.emit_line(")");
            }
            Stmt::While { condition, body } => {
                self.emit_line("(block");
                self.emit_line("(loop");
                let cond = self.generate_expr(condition);
                self.emit(&format!("(br_if 1 (i32.eqz ({})))\n", cond));
                self.generate_stmt(body);
                self.emit_line("(br 0)");
                self.emit_line(")");
                self.emit_line(")");
            }
            Stmt::Print(expr) => {
                let value = self.generate_expr(expr);
                // Determine the type of the expression for correct print function
                let print_func = match self.infer_type(expr) {
                    Some("int") => "$printInt",
                    Some("boolean") => "$printBool",
                    Some("String") => "$printString",
                    _ => "$printInt", // Default to int for now
                };
                // Emit the value first, then the call
                self.emit(&format!("(call {} ({}))\n", print_func, value));
            }
            Stmt::Return { value } => {
                if let Some(expr) = value {
                    let ret_value = self.generate_expr(expr);
                    self.emit(&format!("(return ({}))\n", ret_value));
                } else {
                    self.emit_line("(return)");
                }
            }
            Stmt::Expression(expr) => {
                let value = self.generate_expr(expr);
                self.emit(&format!("({})\n", value));
            }
            _ => {}
        }
    }

    fn generate_expr(&mut self, expr: &Expr) -> String {
        match expr {
            Expr::LiteralInt(n) => format!("i32.const {}", n),
            Expr::LiteralBool(b) => format!("i32.const {}", if *b { 1 } else { 0 }),
            Expr::LiteralString(s) => {
                // Try to use array.new_fixed if available
                if s.is_empty() {
                    // Empty string: just create an empty array
                    format!("array.new $String (i32.const 0) (i32.const 0)")
                } else {
                    let values = s.chars().map(|c| format!("i32.const {}", c as u32)).collect::<Vec<_>>().join(" ");
                    format!("array.new_fixed $String {} {}", s.len(), values)
                }
            }
            Expr::Variable(name) => {
                // Try to find the type of the variable in local_vars_stack
                for locals in self.local_vars_stack.iter().rev() {
                    if let Some(var_type) = locals.get(name) {
                        match var_type.as_str() {
                            "int" | "boolean" => return format!("local.get ${}", name),
                            "String" => return format!("local.get ${}", name),
                            s if self.classes.contains_key(s) => return format!("local.get ${}", name),
                            _ => panic!("Unknown local variable type: {} for {}", var_type, name),
                        }
                    }
                }
                // If not found in locals, check if it's a class name (shouldn't happen for variables)
                panic!("Unknown variable: {}", name)
            }
            Expr::BinaryOp { op, left, right } => {
                if op == "=" {
                    // Assignment expression: left must be a variable or field access
                    if let Expr::Variable(name) = &**left {
                        let value = self.generate_expr(right);
                        format!("local.set ${} ({})\n", name, value)
                    } else if let Expr::FieldAccess { object, field } = &**left {
                        let obj = self.generate_expr(object);
                        let value = self.generate_expr(right);
                        // Use struct.set for field assignment
                        let class_name = self.get_class_name(object);
                        format!("struct.set ${} ${} ({}) ({})\n", class_name, field, obj, value)
                    } else {
                        panic!("Assignment left side must be a variable or field");
                    }
                } else {
                    let left_expr = self.generate_expr(left);
                    let right_expr = self.generate_expr(right);
                    let op_code = match op.as_str() {
                        "+" => "i32.add",
                        "-" => "i32.sub",
                        "*" => "i32.mul",
                        "/" => "i32.div_s",
                        "==" => "i32.eq",
                        "!=" => "i32.ne",
                        "<" => "i32.lt_s",
                        ">" => "i32.gt_s",
                        "<=" => "i32.le_s",
                        ">=" => "i32.ge_s",
                        "&&" => "i32.and",
                        "||" => "i32.or",
                        _ => panic!("Unsupported operator: {}", op),
                    };
                    // Emit operands first, then operator
                    format!("{} ({}) ({})", op_code, left_expr, right_expr)
                }
            }
            Expr::UnaryOp { op, operand } => {
                let operand_expr = self.generate_expr(operand);
                match op.as_str() {
                    "-" => format!("i32.neg {}", operand_expr),
                    "!" => format!("i32.eqz {}", operand_expr),
                    _ => panic!("Unsupported unary operator: {}", op),
                }
            }
            Expr::FieldAccess { object, field } => {
                let obj = self.generate_expr(object);
                // Emit as S-expression: (struct.get $Type $field (<object_expr>))
                format!("struct.get ${} ${} ({})", self.get_class_name(object), field, obj)
            }
            Expr::MethodCall { object, method, args } => {
                let obj_code = self.generate_expr(object);
                let mut call_args = String::new();
                for arg in args {
                    call_args.push_str(&format!("({}) ", self.generate_expr(arg)));
                }
                let class_name = self.get_class_name(object);
                format!("call ${}.{} ({}) {}", class_name, method, obj_code, call_args)
            }
            Expr::New { class_name, args } => {
                let mut init_args = String::new();
                if args.is_empty() {
                    // Zero-initialize all fields if no args provided
                    if let Some(fields) = self.classes.get(class_name) {
                        for _ in fields {
                            init_args.push_str("i32.const 0 ");
                        }
                    }
                } else {
                    for arg in args {
                        init_args.push_str(&format!("{} ", self.generate_expr(arg)));
                    }
                }
                format!("struct.new ${} {}", class_name, init_args)
            }
            Expr::This => "local.get $this".to_string(),
            Expr::FunctionCall { name, args } => {
                let mut call_args = String::new();
                for arg in args {
                    call_args.push_str(&format!("({}) \n", self.generate_expr(arg)));
                }
                // Emit arguments, then call
                format!("call ${} {}", name, call_args)
            },
            _ => "".to_string(),
        }
    }

    fn get_class_name(&self, expr: &Expr) -> String {
        match expr {
            Expr::Variable(name) => {
                // Try to find the type of the variable in local_vars_stack
                for locals in self.local_vars_stack.iter().rev() {
                    if let Some(var_type) = locals.get(name) {
                        if self.classes.contains_key(var_type) {
                            return var_type.clone();
                        }
                    }
                }
                // If not found in locals, check if it's a class name
                if self.classes.contains_key(name) {
                    return name.clone();
                }
                "Object".to_string()
            }
            Expr::This => {
                self.current_class.clone().unwrap_or_else(|| "Object".to_string())
            },
            Expr::FieldAccess { object, .. } => self.get_class_name(object),
            _ => "Object".to_string(),
        }
    }

    fn type_to_wasm(&self, t: &str) -> String {
        match t {
            "int" => "i32".to_string(),
            "boolean" => "i32".to_string(),
            "String" => "(ref $String)".to_string(),
            "void" => "".to_string(),
            s if self.classes.contains_key(s) => {
                format!("(ref ${})", s)
            }
            _ => "(ref $Object)".to_string(),
        }
    }

    fn find_main(&self, stmts: &[Stmt]) -> Option<Box<Stmt>> {
        for stmt in stmts {
            if let Stmt::FunctionDecl { name, body, .. } = stmt {
                if name == "main" {
                    return Some(body.clone());
                }
            }
        }
        None
    }

    fn emit(&self, s: &str) {
        self.output.borrow_mut().push_str(&"  ".repeat(self.indent_level));
        self.output.borrow_mut().push_str(s);
    }

    fn emit_line(&mut self, s: &str) {
        self.emit(s);
        self.output.borrow_mut().push('\n');
    }

    // Helper to collect all local variable declarations in a function/method body
    fn collect_locals(&self, stmt: &Stmt, param_names: &std::collections::HashSet<String>) -> Vec<(String, String)> {
        let mut locals = Vec::new();
        self.collect_locals_recursive(stmt, param_names, &mut locals);
        locals
    }

    fn collect_locals_recursive(&self, stmt: &Stmt, param_names: &std::collections::HashSet<String>, locals: &mut Vec<(String, String)>) {
        match stmt {
            Stmt::Block(stmts) => {
                for s in stmts {
                    self.collect_locals_recursive(s, param_names, locals);
                }
            }
            Stmt::VariableDecl { var_type, name, .. } => {
                if !param_names.contains(name) && !locals.iter().any(|(n, _)| n == name) {
                    locals.push((name.clone(), var_type.clone()));
                }
            }
            // Recurse into branches
            Stmt::If { then_block, else_block, .. } => {
                self.collect_locals_recursive(then_block, param_names, locals);
                if let Some(else_block) = else_block {
                    self.collect_locals_recursive(else_block, param_names, locals);
                }
            }
            Stmt::While { body, .. } => {
                self.collect_locals_recursive(body, param_names, locals);
            }
            Stmt::Expression(_) | Stmt::Assignment { .. } | Stmt::Print(_) | Stmt::Return { .. } => {}
            _ => {}
        }
    }

    // Add a simple type inference helper for print
    fn infer_type(&self, expr: &Expr) -> Option<&str> {
        match expr {
            Expr::LiteralInt(_) => Some("int"),
            Expr::LiteralBool(_) => Some("boolean"),
            Expr::LiteralString(_) => Some("String"),
            Expr::Variable(name) => {
                // Try to find the type from local_vars_stack or classes
                // For now, default to int
                Some("int")
            }
            Expr::BinaryOp { .. } => Some("int"),
            Expr::UnaryOp { .. } => Some("int"),
            Expr::FunctionCall { .. } => Some("int"),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use std::sync::{self, Arc, Mutex};

    use super::*;
    use crate::parser::{Lexer, Parser};
    use wasmtime::{ArrayRef, Config, Engine, ExternRef, Linker, Module, Rooted, Store};
    use wat::parse_str;

    fn parse(input: &str) -> Vec<Stmt> {
        let mut lexer = Lexer::new(input);
        let mut tokens = Vec::new();
        loop {
            let token = lexer.next_token();
            if token == crate::parser::Token::EndOfInput {
                break;
            }
            tokens.push(token);
        }
        let mut parser = Parser::new(tokens);
        parser.parse().unwrap()
    }

    #[test]
    fn codegen_produces_valid_wasm() {
        let input = r#"
            class Program {
                void main() {
                    int x = 10;
                    print(x);
                }
            }
        "#;
        let ast = parse(input);
        let mut codegen = CodeGenerator::new();
        let wat = codegen.generate(ast);
        // wat::parse_str will error if the WAT is invalid
        parse_str(&wat).expect("Generated WAT should be valid");
    }

    #[test]
    fn codegen_valid_wasm_with_class_and_method() {
        let input = r#"
            class Counter {
                int value;
                void inc() { this.value = this.value + 1; }
            }
            class Program {
                void main() {
                    Counter c = new Counter();
                    c.inc();
                    print(c.value);
                }
            }
        "#;
        let ast = parse(input);
        let mut codegen = CodeGenerator::new();
        let wat = codegen.generate(ast);
        parse_str(&wat).expect("Generated WAT should be valid for class and method");
    }

    #[test]
    fn codegen_valid_wasm_with_if_else() {
        let input = r#"
            void main() {
                int x = 5;
                if (x > 0) {
                    print(x);
                } else {
                    print(0);
                }
            }
        "#;
        let ast = parse(input);
        let mut codegen = CodeGenerator::new();
        let wat = codegen.generate(ast);
        parse_str(&wat).expect("Generated WAT should be valid for if/else");
    }

    #[test]
    fn codegen_valid_wasm_with_while() {
        let input = r#"
            void main() {
                int x = 0;
                while (x < 10) {
                    print(x);
                    x = x + 1;
                }
            }
        "#;
        let ast = parse(input);
        let mut codegen = CodeGenerator::new();
        let wat = codegen.generate(ast);
        parse_str(&wat).expect("Generated WAT should be valid for while loop");
    }

    #[test]
    fn codegen_valid_wasm_with_return() {
        let input = r#"
            int add(int a, int b) {
                return a + b;
            }
            void main() {
                int y = add(1, 2);
                print(y);
            }
        "#;
        let ast = parse(input);
        let mut codegen = CodeGenerator::new();
        let wat = codegen.generate(ast);
        parse_str(&wat).expect("Generated WAT should be valid for return statement");
    }

    #[test]
    fn codegen_valid_wasm_with_function_call() {
        let input = r#"
            int square(int x) {
                return x * x;
            }
            void main() {
                int z = square(4);
                print(z);
            }
        "#;
        let ast = parse(input);
        let mut codegen = CodeGenerator::new();
        let wat = codegen.generate(ast);
        parse_str(&wat).expect("Generated WAT should be valid for function call");
    }

    fn compile_and_run(input: &str) -> String {
        use wasmtime::{Engine, Module, Store, Linker, Config};
        use std::sync::{Arc, Mutex};
        let ast = parse(input);
        let mut codegen = CodeGenerator::new();
        let wat = codegen.generate(ast);
        eprintln!("{}", &wat);
        let wasm = wat::parse_str(&wat).expect("Generated WAT should be valid");

        // Setup wasmtime
        let mut config = Config::new();
        config.wasm_multi_memory(true);
        config.wasm_gc(true);
        let engine = Engine::new(&config).unwrap();
        let module = Module::from_binary(&engine, &wasm).unwrap();
        let mut linker = Linker::new(&engine);
        let mut store = Store::new(&engine, ());

        // Capture print output
        let output = Arc::new(Mutex::new(Vec::<i32>::new()));

        linker.func_wrap("host", "printInt", {
            let output = output.clone();
            move |val: i32| {
                output.lock().unwrap().push(val);
            }
        }).unwrap();
        linker.func_wrap("host", "printBool", {
            let output = output.clone();
            move |val: i32| {
                output.lock().unwrap().push(val);
            }
        }).unwrap();
        linker.func_wrap("host", "printFloat", {
            let _output = output.clone();
            move |_val: f32| {
            }
        }).unwrap();
        linker.func_wrap("host", "printString", {
            move |_val: Rooted<ArrayRef>| {
            }
        }).unwrap();

        let instance = linker.instantiate(&mut store, &module).unwrap();
        let main = instance.get_func(&mut store, "main").expect("main function");
        main.call(&mut store, &[], &mut []).expect("main should run");

        let out = output.lock().unwrap();
        out.iter().map(|v| v.to_string()).collect::<Vec<_>>().join(", ")
    }

    #[test]
    fn codegen_execution_with_wasmtime() {
        let input = r#"
                void main() {
                    int x = 42;
                    print(x + 1);
                }
        "#;
        let result = compile_and_run(input);
        assert_eq!(result, "43");
    }

    #[test]
    fn codegen_execution_print_bool() {
        let input = r#"
            void main() {
                print(true);
            }
        "#;
        let result = compile_and_run(input);
        assert_eq!(result, "1");
    }

    #[test]
    fn codegen_execution_print_string() {
        let input = r#"
            void main() {
                print("hi");
            }
        "#;
        let result = compile_and_run(input);
    }

    #[test]
    fn codegen_execution_print_function_result() {
        let input = r#"
            int add(int a, int b) {
                return a + b;
            }
            void main() {
                print(add(2, 3));
            }
        "#;
        let result = compile_and_run(input);
        assert_eq!(result, "5");
        
    }

    #[test]
    fn codegen_execution_print_class_field() {
        let input = r#"
            class Foo {
                int value;
            }
            void main() {
                Foo f = new Foo();
                f.value = 99;
                print(f.value);
            }
        "#;
        let result = compile_and_run(input);
        assert_eq!(result, "99");
    }

    #[test]
    fn codegen_execution_complex_program() {
        let input = r#"
            class Accumulator {
                int sum;
                void add(int x) {
                    this.sum = this.sum + x;
                }
            }
            int factorial(int n) {
                int result = 1;
                while (n > 1) {
                    result = result * n;
                    n = n - 1;
                }
                return result;
            }
            void main() {
                Accumulator acc = new Accumulator();
                int i = 1;
                while (i <= 5) {
                    acc.add(factorial(i));
                    i = i + 1;
                }
                print(acc.sum); // Should print 153 (1!+2!+3!+4!+5!)
            }
        "#;
        let result = compile_and_run(input);
        assert_eq!(result, "153");
    }
}