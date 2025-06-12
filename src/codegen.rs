// codegen.rs

use crate::parser::{Stmt, Expr, Token};
use std::collections::{HashMap, HashSet};
use std::cell::RefCell;

pub struct CodeGenerator {
    classes: HashMap<String, Vec<(String, String)>>,
    output: RefCell<String>,
    indent_level: usize,
    local_vars_stack: Vec<HashMap<String, String>>, // Stack of local variable names to types
    current_class: Option<String>,
    current_return_type: Option<String>, // Track current function's return type
    wasi_mode: bool, // Whether to generate WASI-compatible output
}

impl CodeGenerator {
    pub fn new(wasi_mode: bool) -> Self {
        CodeGenerator {
            classes: HashMap::new(),
            output: RefCell::new(String::new()),
            indent_level: 0,
            local_vars_stack: Vec::new(),
            current_class: None,
            current_return_type: None,
            wasi_mode,
        }
    }

    pub fn generate(&mut self, stmts: Vec<Stmt>) -> String {
        self.output.borrow_mut().push_str("(module\n");
        self.indent_level += 1;

        if self.wasi_mode {
            // Import WASI fd_write function
            self.emit_line("(import \"wasi_snapshot_preview1\" \"fd_write\"");
            self.emit_line("  (func $fd_write (param i32 i32 i32 i32) (result i32)))");
            
            // Define memory and export it
            self.emit_line("(memory 1)");
            self.emit_line("(export \"memory\" (memory 0))");
            
            // Global memory pointer for string formatting
            self.emit_line("(global $memory_offset (mut i32) (i32.const 1024))");
        } else {
            // Import print functions (original behavior)
            self.emit_line("(import \"host\" \"printInt\" (func $printInt (param i32)))");
            self.emit_line("(import \"host\" \"printBool\" (func $printBool (param i32)))");
            self.emit_line("(import \"host\" \"printString\" (func $printString (param (ref array))))");
            self.emit_line("(import \"host\" \"printFloat\" (func $printFloat (param f32)))");
            self.emit_line("(import \"host\" \"printlnInt\" (func $printlnInt (param i32)))");
            self.emit_line("(import \"host\" \"printlnBool\" (func $printlnBool (param i32)))");
            self.emit_line("(import \"host\" \"printlnString\" (func $printlnString (param (ref array))))");
            self.emit_line("(import \"host\" \"printlnFloat\" (func $printlnFloat (param f32)))");
        }

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

        // Define array types for any used array types
        let mut array_types = std::collections::HashSet::new();
        for stmt in &stmts {
            self.collect_array_types(stmt, &mut array_types);
        }
        for array_type in &array_types {
            let elem_type = if array_type.starts_with("int") {
                "i32".to_string()
            } else if array_type.starts_with("boolean") {
                "i32".to_string()
            } else if array_type.starts_with("String") {
                "(ref null $String)".to_string()
            } else if self.classes.contains_key(&array_type[..array_type.len()-2]) {
                format!("(ref null ${})", &array_type[..array_type.len()-2])
            } else {
                "(ref null $Object)".to_string()
            };
            let type_name = format!("${}Array", &array_type[..array_type.len()-2]);
            self.emit_line(&format!("(type {} (array (mut {})))", type_name, elem_type));
        }

        // Generate WASI helper functions if in WASI mode
        if self.wasi_mode {
            self.generate_wasi_helper_functions();
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
        if let Some(_main) = self.find_main(&stmts) {
            if self.wasi_mode {
                self.emit_line("(export \"_start\" (func $main))");
            } else {
                self.emit_line("(export \"main\" (func $main))");
            }
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
        let prev_return_type = self.current_return_type.clone();
        self.current_return_type = Some(return_type.to_string());
        
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
        } else {
            // Check if function has proper return coverage for non-void functions
            if !self.has_guaranteed_return(body) {
                panic!("Compile error: Function '{}' with return type '{}' does not have a return statement on all code paths", name, return_type);
            }
            self.emit_line("unreachable");
        }
        self.local_vars_stack.pop();
        self.indent_level -= 1;
        self.emit_line(")");
        self.current_return_type = prev_return_type;
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
        let prev_return_type = self.current_return_type.clone();
        self.current_class = Some(class_name.to_string());
        self.current_return_type = Some(return_type.to_string());
        self.emit(&format!("(func ${}.{}", class_name, method_name));
        self.emit(&format!(" (param $this (ref null ${}))", class_name));
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
        } else {
            // Check if method has proper return coverage for non-void methods
            if !self.has_guaranteed_return(body) {
                panic!("Compile error: Method '{}.{}' with return type '{}' does not have a return statement on all code paths", class_name, method_name, return_type);
            }
            self.emit_line("unreachable");
        }
        self.local_vars_stack.pop();
        self.indent_level -= 1;
        self.emit_line(")");
        // Export the method so it can be called
        self.emit_line(&format!("(export \"{}.{}\" (func ${}.{}))", class_name, method_name, class_name, method_name));
        self.current_class = prev_class;
        self.current_return_type = prev_return_type;
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
                    let value = self.generate_expr(&expr);
                    self.emit(&format!("(local.set ${} ({}))\n", name, value));
                }
            }
            Stmt::Assignment { name, value } => {
                let value = self.generate_expr(value);
                self.emit(&format!("(global.set ${} ({}))\n", name, value));
            }
            Stmt::If { condition, then_block, else_block } => {
                
                // Generate regular if statement  
                let cond = self.generate_expr(condition);
                self.emit(&format!("(if ({})\n", cond));
                self.indent_level += 1;
                self.emit_line("(then");
                self.indent_level += 1;
                self.generate_stmt(then_block);
                self.indent_level -= 1;
                self.emit_line(")");
                if let Some(else_block) = else_block {
                    self.emit_line("(else");
                    self.indent_level += 1;
                    self.generate_stmt(else_block);
                    self.indent_level -= 1;
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
            Stmt::For { init, condition, increment, body } => {
                // Generate init statement if present
                if let Some(init_stmt) = init {
                    self.generate_stmt(init_stmt);
                }
                
                // Generate the loop structure
                self.emit_line("(block");
                self.emit_line("(loop");
                
                // Generate condition check if present (default to true if none)
                if let Some(cond) = condition {
                    let cond = self.generate_expr(cond);
                    self.emit(&format!("(br_if 1 (i32.eqz ({})))\n", cond));
                }
                
                // Generate body
                self.generate_stmt(body);
                
                // Generate increment if present
                if let Some(inc) = increment {
                    let inc_code = self.generate_expr(inc);
                    self.emit(&format!("({})\n", inc_code));
                }
                
                // Loop back
                self.emit_line("(br 0)");
                self.emit_line(")");
                self.emit_line(")");
            }
            Stmt::Print(expr) => {
                let value = self.generate_expr(expr);
                if self.wasi_mode {
                    // In WASI mode, use WASI print functions
                    let print_func = match self.infer_type(expr) {
                        Some("int") => "$wasiPrintInt",
                        Some("boolean") => "$wasiPrintBool", 
                        Some("String") => "$wasiPrintString",
                        _ => "$wasiPrintInt", // Default to int for now
                    };
                    self.emit(&format!("(call {} ({}))\n", print_func, value));
                } else {
                    // Original host function approach
                    let print_func = match self.infer_type(expr) {
                        Some("int") => "$printInt",
                        Some("boolean") => "$printBool",
                        Some("String") => "$printString",
                        _ => "$printInt", // Default to int for now
                    };
                    self.emit(&format!("(call {} ({}))\n", print_func, value));
                }
            }
            Stmt::Println(expr) => {
                let value = self.generate_expr(expr);
                if self.wasi_mode {
                    // In WASI mode, use WASI println functions
                    let println_func = match self.infer_type(expr) {
                        Some("int") => "$wasiPrintlnInt",
                        Some("boolean") => "$wasiPrintlnBool",
                        Some("String") => "$wasiPrintlnString",
                        _ => "$wasiPrintlnInt", // Default to int for now
                    };
                    self.emit(&format!("(call {} ({}))\n", println_func, value));
                } else {
                    // Original host function approach
                    let println_func = match self.infer_type(expr) {
                        Some("int") => "$printlnInt",
                        Some("boolean") => "$printlnBool",
                        Some("String") => "$printlnString",
                        _ => "$printlnInt", // Default to int for now
                    };
                    self.emit(&format!("(call {} ({}))\n", println_func, value));
                }
            }
            Stmt::Return { value } => {
                if let Some(expr) = value {
                    let ret_value = if matches!(expr, Expr::LiteralNull) {
                        // Use the current return type for null literals
                        if let Some(return_type) = &self.current_return_type {
                            self.generate_null_ref(return_type)
                        } else {
                            self.generate_expr(expr)
                        }
                    } else {
                        self.generate_expr(expr)
                    };
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
            Expr::LiteralNull => "ref.null $String".to_string(),
            Expr::Variable(name) => {
                // Try to find the type of the variable in local_vars_stack
                for locals in self.local_vars_stack.iter().rev() {
                    if let Some(var_type) = locals.get(name) {
                        match var_type.as_str() {
                            "int" | "boolean" | "String" => return format!("local.get ${}", name),
                            s if self.classes.contains_key(s) => return format!("local.get ${}", name),
                            s if s.ends_with("[]") => return format!("local.get ${}", name),
                            _ => panic!("Unknown local variable type: {} for {}", var_type, name),
                        }
                    }
                }
                // If not found in locals, check if it's a class name (shouldn't happen for variables)
                panic!("Unknown variable: {}", name)
            }
            Expr::BinaryOp { op, left, right } => {
                if op == "=" {
                    // Assignment expression: left must be a variable, field, or array access
                    if let Expr::Variable(name) = &**left {
                        let value = self.generate_expr(right);
                        format!("local.set ${} ({})\n", name, value)
                    } else if let Expr::FieldAccess { object, field } = &**left {
                        let obj = self.generate_expr(object);
                        // For field assignments with null, use the correct type
                        let value = if matches!(**right, Expr::LiteralNull) {
                            let class_name = self.get_class_name(object);
                            if let Some(field_type) = self.get_field_type(&class_name, field) {
                                self.generate_null_ref(&field_type)
                            } else {
                                self.generate_expr(right)
                            }
                        } else {
                            self.generate_expr(right)
                        };
                        // Use struct.set for field assignment
                        let class_name = self.get_class_name(object);
                        format!("struct.set ${} ${} ({}) ({})\n", class_name, field, obj, value)
                    } else if let Expr::ArrayAccess { array, index } = &**left {
                        let arr = self.generate_expr(array);
                        let idx = self.generate_expr(index);
                        let value = self.generate_expr(right);
                        // Infer the array type from the variable type
                        let arr_type = self.infer_array_type(array);
                        format!("array.set {} ({}) ({}) ({})\n", arr_type, arr, idx, value)
                    } else {
                        panic!("Assignment left side must be a variable, field, or array element");
                    }
                } else {
                    let left_expr = self.generate_expr(left);
                    let right_expr = self.generate_expr(right);
                    
                    // For equality/inequality operations, we need to determine if we're comparing references or integers
                    let op_code = match op.as_str() {
                        "+" => "i32.add",
                        "-" => "i32.sub",
                        "*" => "i32.mul",
                        "/" => "i32.div_s",
                        "==" => {
                            // Check if we're comparing references (String, objects, or null)
                            let left_type = self.infer_type(left);
                            let right_type = self.infer_type(right);
                            if matches!(left_type, Some("String") | Some("Object")) || 
                               matches!(right_type, Some("String") | Some("Object")) ||
                               matches!(**left, Expr::LiteralNull) || matches!(**right, Expr::LiteralNull) {
                                
                                // Handle null comparisons with correct types
                                let left_expr = if matches!(**left, Expr::LiteralNull) {
                                    // If right side is a field access, use its type for the null
                                    if let Expr::FieldAccess { object, field } = &**right {
                                        let class_name = self.get_class_name(object);
                                        if let Some(field_type) = self.get_field_type(&class_name, field) {
                                            self.generate_null_ref(&field_type)
                                        } else {
                                            self.generate_expr(left)
                                        }
                                    } else {
                                        self.generate_expr(left)
                                    }
                                } else {
                                    self.generate_expr(left)
                                };
                                
                                let right_expr = if matches!(**right, Expr::LiteralNull) {
                                    // If left side is a field access, use its type for the null
                                    if let Expr::FieldAccess { object, field } = &**left {
                                        let class_name = self.get_class_name(object);
                                        if let Some(field_type) = self.get_field_type(&class_name, field) {
                                            self.generate_null_ref(&field_type)
                                        } else {
                                            self.generate_expr(right)
                                        }
                                    } else {
                                        self.generate_expr(right)
                                    }
                                } else {
                                    self.generate_expr(right)
                                };
                                
                                return format!("ref.eq ({}) ({})", left_expr, right_expr);
                            } else {
                                "i32.eq"
                            }
                        },
                        "!=" => {
                            // Check if we're comparing references (String, objects, or null)
                            let left_type = self.infer_type(left);
                            let right_type = self.infer_type(right);
                            if matches!(left_type, Some("String") | Some("Object")) || 
                               matches!(right_type, Some("String") | Some("Object")) ||
                               matches!(**left, Expr::LiteralNull) || matches!(**right, Expr::LiteralNull) {
                                
                                // Handle null comparisons with correct types
                                let left_expr = if matches!(**left, Expr::LiteralNull) {
                                    // If right side is a field access, use its type for the null
                                    if let Expr::FieldAccess { object, field } = &**right {
                                        let class_name = self.get_class_name(object);
                                        if let Some(field_type) = self.get_field_type(&class_name, field) {
                                            self.generate_null_ref(&field_type)
                                        } else {
                                            self.generate_expr(left)
                                        }
                                    } else {
                                        self.generate_expr(left)
                                    }
                                } else {
                                    self.generate_expr(left)
                                };
                                
                                let right_expr = if matches!(**right, Expr::LiteralNull) {
                                    // If left side is a field access, use its type for the null
                                    if let Expr::FieldAccess { object, field } = &**left {
                                        let class_name = self.get_class_name(object);
                                        if let Some(field_type) = self.get_field_type(&class_name, field) {
                                            self.generate_null_ref(&field_type)
                                        } else {
                                            self.generate_expr(right)
                                        }
                                    } else {
                                        self.generate_expr(right)
                                    }
                                } else {
                                    self.generate_expr(right)
                                };
                                
                                // For ref.ne, we need to negate ref.eq
                                return format!("i32.eqz (ref.eq ({}) ({}))", left_expr, right_expr);
                            } else {
                                "i32.ne"
                            }
                        },
                        "<" => "i32.lt_s",
                        ">" => "i32.gt_s",
                        "<=" => "i32.le_s",
                        ">=" => "i32.ge_s",
                        "&&" => "i32.and",  // Note: This is actually bitwise, logical AND needs different handling
                        "||" => "i32.or",   // Note: This is actually bitwise, logical OR needs different handling
                        "&" => "i32.and",   // Bitwise AND
                        "|" => "i32.or",    // Bitwise OR
                        "^" => "i32.xor",   // Bitwise XOR
                        "<<" => "i32.shl",  // Left shift
                        ">>" => "i32.shr_s", // Signed right shift
                        ">>>" => "i32.shr_u", // Unsigned right shift
                        _ => panic!("Unsupported operator: {}", op),
                    };
                    // Emit operands first, then operator
                    format!("{} ({}) ({})", op_code, left_expr, right_expr)
                }
            }
            Expr::UnaryOp { op, operand } => {
                let operand_expr = self.generate_expr(operand);
                match op.as_str() {
                    "-" => format!("i32.sub (i32.const 0) ({})", operand_expr),
                    "!" => format!("i32.eqz ({})", operand_expr),
                    "~" => format!("i32.xor (i32.const -1) ({})", operand_expr), // Bitwise NOT: XOR with all 1s
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
                if class_name.ends_with("[]") && args.len() == 1 {
                    // new int[10] style
                    let elem_type = &class_name[..class_name.len() - 2];
                    let type_name = format!("${}Array", elem_type);
                    let len_expr = self.generate_expr(&args[0]);
                    // Default initialize to 0 or null
                    let default_val = match elem_type {
                        "int" | "boolean" => "(i32.const 0)".to_string(),
                        "String" => "(ref.null $String)".to_string(),
                        s if self.classes.contains_key(s) => format!("(ref.null ${})", s),
                        _ => "(ref.null $Object)".to_string(),
                    };
                    format!("array.new {} {} {}", type_name, default_val, len_expr)
                } else {
                    let mut init_args = String::new();
                    if args.is_empty() {
                        if let Some(fields) = self.classes.get(class_name) {
                            for (_, field_type) in fields {
                                let default_val = match field_type.as_str() {
                                    "int" | "boolean" => "i32.const 0".to_string(),
                                    "String" => "ref.null $String".to_string(),
                                    s if self.classes.contains_key(s) => format!("ref.null ${}", s),
                                    _ => "ref.null $Object".to_string(),
                                };
                                init_args.push_str(&format!("{} ", default_val));
                            }
                        }
                    } else {
                        for arg in args {
                            init_args.push_str(&format!("{} ", self.generate_expr(arg)));
                        }
                    }
                    format!("struct.new ${} {}", class_name, init_args)
                }
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
            Expr::ArrayAccess { array, index } => {
                let arr = self.generate_expr(array);
                let idx = self.generate_expr(index);
                let arr_type = self.infer_array_type(array);
                format!("array.get {} ({}) ({})", arr_type, arr, idx)
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
            Expr::ArrayAccess { array, .. } => {
                // For array access, get the element type
                if let Expr::Variable(array_name) = &**array {
                    for locals in self.local_vars_stack.iter().rev() {
                        if let Some(var_type) = locals.get(array_name) {
                            if var_type.ends_with("[]") {
                                let elem_type = &var_type[..var_type.len() - 2];
                                if self.classes.contains_key(elem_type) {
                                    return elem_type.to_string();
                                }
                            }
                        }
                    }
                }
                "Object".to_string()
            }
            _ => "Object".to_string(),
        }
    }

    fn type_to_wasm(&self, t: &str) -> String {
        if t.ends_with("[]") {
            let elem_type = &t[..t.len() - 2];
            let wasm_elem_type = match elem_type {
                "int" => "i32",
                "boolean" => "i32",
                "String" => "(ref $String)",
                s if self.classes.contains_key(s) => &format!("(ref ${})", s),
                _ => "(ref $Object)",
            };
            // Define a type name for the array type
            let type_name = format!("${}Array", elem_type);
            format!("(ref {})", type_name)
        } else {
            match t {
                "int" => "i32".to_string(),
                "boolean" => "i32".to_string(),
                "String" => "(ref null $String)".to_string(),
                "void" => "".to_string(),
                s if self.classes.contains_key(s) => {
                    format!("(ref null ${})", s)
                }
                _ => "(ref null $Object)".to_string(),
            }
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
            Stmt::For { init, body, .. } => {
                if let Some(init_stmt) = init {
                    self.collect_locals_recursive(init_stmt, param_names, locals);
                }
                self.collect_locals_recursive(body, param_names, locals);
            }
            Stmt::Expression(_) | Stmt::Assignment { .. } | Stmt::Print(_) | Stmt::Println(_) | Stmt::Return { .. } => {}
            _ => {}
        }
    }

    // Add a simple type inference helper for print
    fn infer_type(&self, expr: &Expr) -> Option<&str> {
        match expr {
            Expr::LiteralInt(_) => Some("int"),
            Expr::LiteralBool(_) => Some("boolean"),
            Expr::LiteralString(_) => Some("String"),
            Expr::LiteralNull => Some("Object"), // null can be assigned to any reference type
            Expr::Variable(name) => {
                // Try to find the type from local_vars_stack
                for locals in self.local_vars_stack.iter().rev() {
                    if let Some(var_type) = locals.get(name) {
                        return Some(var_type);
                    }
                }
                // Default to int if not found
                Some("int")
            }
            Expr::BinaryOp { .. } => Some("int"),
            Expr::UnaryOp { .. } => Some("int"),
            Expr::FunctionCall { .. } => Some("int"),
            _ => None,
        }
    }

    fn collect_array_types(&self, stmt: &Stmt, set: &mut std::collections::HashSet<String>) {
        match stmt {
            Stmt::VariableDecl { var_type, .. } | Stmt::FieldDecl { var_type, .. } => {
                if var_type.ends_with("[]") {
                    set.insert(var_type.clone());
                }
            }
            Stmt::FunctionDecl { return_type, params, body, .. } => {
                if return_type.ends_with("[]") {
                    set.insert(return_type.clone());
                }
                for (param_type, _) in params {
                    if param_type.ends_with("[]") {
                        set.insert(param_type.clone());
                    }
                }
                self.collect_array_types(body, set);
            }
            Stmt::ClassDecl { fields, methods, .. } => {
                for f in fields {
                    self.collect_array_types(f, set);
                }
                for m in methods {
                    self.collect_array_types(m, set);
                }
            }
            Stmt::Block(stmts) => {
                for s in stmts {
                    self.collect_array_types(s, set);
                }
            }
            Stmt::If { then_block, else_block, .. } => {
                self.collect_array_types(then_block, set);
                if let Some(else_block) = else_block {
                    self.collect_array_types(else_block, set);
                }
            }
            Stmt::While { body, .. } => {
                self.collect_array_types(body, set);
            }
            Stmt::For { init, body, .. } => {
                if let Some(init_stmt) = init {
                    self.collect_array_types(init_stmt, set);
                }
                self.collect_array_types(body, set);
            }
            _ => {}
        }
    }

    // Helper to infer array type for array.set
    fn infer_array_type(&self, array_expr: &Expr) -> String {
        match array_expr {
            Expr::Variable(name) => {
                for locals in self.local_vars_stack.iter().rev() {
                    if let Some(var_type) = locals.get(name) {
                        if var_type.ends_with("[]") {
                            let elem_type = &var_type[..var_type.len() - 2];
                            return format!("${}Array", elem_type);
                        }
                    }
                }
                "$Array".to_string()
            }
            _ => "$Array".to_string(),
        }
    }

    // Check if a statement guarantees a return on all execution paths
    fn has_guaranteed_return(&self, stmt: &Stmt) -> bool {
        match stmt {
            Stmt::Return { .. } => true,
            Stmt::If { then_block, else_block, .. } => {
                // Both branches must guarantee a return
                if let Some(else_stmt) = else_block {
                    self.has_guaranteed_return(then_block) && self.has_guaranteed_return(else_stmt)
                } else {
                    false // No else branch means no guaranteed return
                }
            }
            Stmt::Block(stmts) => {
                // Check if any statement in the block guarantees a return
                stmts.iter().any(|s| self.has_guaranteed_return(s))
            }
            Stmt::While { .. } => false, // While loops don't guarantee execution
            Stmt::For { .. } => false,   // For loops don't guarantee execution
            _ => false,
        }
    }

    fn get_field_type(&self, class_name: &str, field_name: &str) -> Option<String> {
        if let Some(fields) = self.classes.get(class_name) {
            for (name, field_type) in fields {
                if name == field_name {
                    return Some(field_type.clone());
                }
            }
        }
        None
    }

    fn generate_null_ref(&self, expected_type: &str) -> String {
        match expected_type {
            "int" | "boolean" => "i32.const 0".to_string(),
            "String" => "ref.null $String".to_string(),
            s if self.classes.contains_key(s) => format!("ref.null ${}", s),
            _ => "ref.null $Object".to_string(),
        }
    }

    fn generate_wasi_helper_functions(&mut self) {
        // Helper function to write a string to stdout using WASI fd_write
        self.emit_line("(func $wasiWriteString (param $ptr i32) (param $len i32)");
        self.indent_level += 1;
        self.emit_line("(local $iovec_ptr i32)");
        self.emit_line("(local $nwritten_ptr i32)");
        
        // Ensure current memory offset is 4-byte aligned
        self.emit_line("(global.set $memory_offset");
        self.emit_line("  (i32.and (i32.add (global.get $memory_offset) (i32.const 3)) (i32.const -4)))");
        
        // Allocate space for iovec structure (8 bytes) and nwritten (4 bytes)
        self.emit_line("(local.set $iovec_ptr (global.get $memory_offset))");
        self.emit_line("(local.set $nwritten_ptr (i32.add (local.get $iovec_ptr) (i32.const 8)))");
        
        // Update memory offset for next allocation
        self.emit_line("(global.set $memory_offset (i32.add (local.get $nwritten_ptr) (i32.const 4)))");
        
        // Set up iovec structure
        self.emit_line("(i32.store (local.get $iovec_ptr) (local.get $ptr))");
        self.emit_line("(i32.store (i32.add (local.get $iovec_ptr) (i32.const 4)) (local.get $len))");
        
        // Call fd_write
        self.emit_line("(call $fd_write");
        self.emit_line("  (i32.const 1) (local.get $iovec_ptr) (i32.const 1) (local.get $nwritten_ptr))");
        self.emit_line("drop");
        self.indent_level -= 1;
        self.emit_line(")");
        
        // Function to print integers
        self.emit_line("(func $wasiPrintInt (param $val i32)");
        self.indent_level += 1;
        self.emit_line("(local $ptr i32) (local $len i32) (local $temp i32) (local $digit i32) (local $is_negative i32) (local $start i32) (local $end i32) (local $char i32)");
        
        // Allocate aligned memory for string buffer (12 bytes, already 4-byte aligned)
        self.emit_line("(local.set $ptr (global.get $memory_offset))");
        self.emit_line("(global.set $memory_offset (i32.add (global.get $memory_offset) (i32.const 12)))");
        
        // Handle zero
        self.emit_line("(if (i32.eqz (local.get $val))");
        self.emit_line("  (then");
        self.emit_line("    (i32.store8 (local.get $ptr) (i32.const 48))");
        self.emit_line("    (call $wasiWriteString (local.get $ptr) (i32.const 1))");
        self.emit_line("    (return)");
        self.emit_line("  ))");
        
        // Handle negative
        self.emit_line("(local.set $is_negative (i32.lt_s (local.get $val) (i32.const 0)))");
        self.emit_line("(if (local.get $is_negative)");
        self.emit_line("  (then");
        self.emit_line("    (local.set $val (i32.sub (i32.const 0) (local.get $val)))");
        self.emit_line("    (i32.store8 (local.get $ptr) (i32.const 45))");
        self.emit_line("    (local.set $start (i32.const 1))");
        self.emit_line("  )");
        self.emit_line("  (else");
        self.emit_line("    (local.set $start (i32.const 0))");
        self.emit_line("  )");
        self.emit_line(")");
        
        // Convert digits (in reverse order)
        self.emit_line("(local.set $temp (local.get $val))");
        self.emit_line("(local.set $len (local.get $start))");
        self.emit_line("(loop $digit_loop");
        self.emit_line("  (local.set $digit (i32.rem_u (local.get $temp) (i32.const 10)))");
        self.emit_line("  (local.set $temp (i32.div_u (local.get $temp) (i32.const 10)))");
        self.emit_line("  (i32.store8 (i32.add (local.get $ptr) (local.get $len)) (i32.add (local.get $digit) (i32.const 48)))");
        self.emit_line("  (local.set $len (i32.add (local.get $len) (i32.const 1)))");
        self.emit_line("  (br_if $digit_loop (i32.ne (local.get $temp) (i32.const 0)))");
        self.emit_line(")");
        
        // Reverse the digits (they were stored backwards)
        self.emit_line("(local.set $start (local.get $start))");
        self.emit_line("(local.set $end (i32.sub (local.get $len) (i32.const 1)))");
        self.emit_line("(loop $reverse_loop");
        self.emit_line("  (br_if 1 (i32.ge_u (local.get $start) (local.get $end)))");
        self.emit_line("  (local.set $char (i32.load8_u (i32.add (local.get $ptr) (local.get $start))))");
        self.emit_line("  (i32.store8 (i32.add (local.get $ptr) (local.get $start)) (i32.load8_u (i32.add (local.get $ptr) (local.get $end))))");
        self.emit_line("  (i32.store8 (i32.add (local.get $ptr) (local.get $end)) (local.get $char))");
        self.emit_line("  (local.set $start (i32.add (local.get $start) (i32.const 1)))");
        self.emit_line("  (local.set $end (i32.sub (local.get $end) (i32.const 1)))");
        self.emit_line("  (br $reverse_loop)");
        self.emit_line(")");
        
        self.emit_line("(call $wasiWriteString (local.get $ptr) (local.get $len))");
        self.indent_level -= 1;
        self.emit_line(")");
        
        // Function to print booleans
        self.emit_line("(func $wasiPrintBool (param $val i32)");
        self.indent_level += 1;
        self.emit_line("(if (local.get $val)");
        self.emit_line("  (then (call $wasiWriteString (i32.const 512) (i32.const 4)))");  
        self.emit_line("  (else (call $wasiWriteString (i32.const 516) (i32.const 5)))");
        self.emit_line(")");
        self.indent_level -= 1;
        self.emit_line(")");
        
        // Function to print strings
        self.emit_line("(func $wasiPrintString (param $str (ref null $String))");
        self.indent_level += 1;
        self.emit_line("(local $len i32) (local $ptr i32) (local $i i32) (local $aligned_len i32)");
        self.emit_line("(local.set $len (array.len (local.get $str)))");
        self.emit_line("(local.set $ptr (global.get $memory_offset))");
        
        // Align the length to 4-byte boundary to maintain alignment for subsequent allocations
        self.emit_line("(local.set $aligned_len (i32.and (i32.add (local.get $len) (i32.const 3)) (i32.const -4)))");
        self.emit_line("(global.set $memory_offset (i32.add (global.get $memory_offset) (local.get $aligned_len)))");
        
        // Copy string to memory
        self.emit_line("(local.set $i (i32.const 0))");
        self.emit_line("(loop $copy_loop");
        self.emit_line("  (br_if 1 (i32.ge_u (local.get $i) (local.get $len)))");
        self.emit_line("  (i32.store8 (i32.add (local.get $ptr) (local.get $i))");
        self.emit_line("    (array.get_u $String (local.get $str) (local.get $i)))");
        self.emit_line("  (local.set $i (i32.add (local.get $i) (i32.const 1)))");
        self.emit_line("  (br $copy_loop)");
        self.emit_line(")");
        
        self.emit_line("(call $wasiWriteString (local.get $ptr) (local.get $len))");
        self.indent_level -= 1;
        self.emit_line(")");
        
        // Println variants
        self.emit_line("(func $wasiPrintlnInt (param $val i32)");
        self.indent_level += 1;
        self.emit_line("(call $wasiPrintInt (local.get $val))");
        self.emit_line("(call $wasiWriteString (i32.const 520) (i32.const 1))");
        self.indent_level -= 1;
        self.emit_line(")");
        
        self.emit_line("(func $wasiPrintlnBool (param $val i32)");
        self.indent_level += 1;
        self.emit_line("(call $wasiPrintBool (local.get $val))");
        self.emit_line("(call $wasiWriteString (i32.const 520) (i32.const 1))");
        self.indent_level -= 1;
        self.emit_line(")");
        
        self.emit_line("(func $wasiPrintlnString (param $str (ref null $String))");
        self.indent_level += 1;
        self.emit_line("(call $wasiPrintString (local.get $str))");
        self.emit_line("(call $wasiWriteString (i32.const 520) (i32.const 1))");
        self.indent_level -= 1;
        self.emit_line(")");
        
        // Store static strings in memory
        self.emit_line("(data (i32.const 512) \"true\")");
        self.emit_line("(data (i32.const 516) \"false\")");
        self.emit_line("(data (i32.const 520) \"\\n\")");
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use wasmtime::{ArrayRef, Caller, Rooted};
    
    use wat::parse_str;

    fn parse(input: &str) -> Vec<Stmt> {
        let lexer = crate::parser::Lexer::new(input);
        let mut parser = crate::parser::Parser::new(lexer);
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
        let mut codegen = CodeGenerator::new(false);
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
        let mut codegen = CodeGenerator::new(false);
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
        let mut codegen = CodeGenerator::new(false);
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
        let mut codegen = CodeGenerator::new(false);
        let wat = codegen.generate(ast);
        parse_str(&wat).expect("Generated WAT should be valid for while loop");
    }

    #[test]
    fn codegen_execution_with_for_loop() {
        let input = r#"
            void main() {
                for (int i = 0; i < 5; i = i + 1) {
                    print(i);
                }
            }
        "#;
        let result = compile_and_run(input);
        assert_eq!(result, "01234");
    }

    #[test]
    fn codegen_execution_comprehensive_for_loops() {
        let input = r#"
            void main() {
                // Simple counting loop
                for (int i = 0; i < 3; i = i + 1) {
                    print(i);
                }
                
                // Countdown loop
                for (int j = 5; j > 2; j = j - 1) {
                    print(j);
                }
                
                // Nested loops
                for (int x = 0; x < 2; x = x + 1) {
                    for (int y = 0; y < 2; y = y + 1) {
                        print(x * 2 + y);
                    }
                }
                
                // Loop with no init (variable declared outside)
                int k = 7;
                for (; k < 9; k = k + 1) {
                    print(k);
                }
            }
        "#;
        let result = compile_and_run(input);
        // Expected: "012" + "543" + "0123" + "78"
        assert_eq!(result, "012543012378");
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
        let mut codegen = CodeGenerator::new(false);
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
        let mut codegen = CodeGenerator::new(false);
        let wat = codegen.generate(ast);
        parse_str(&wat).expect("Generated WAT should be valid for function call");
    }

    #[test]
    fn codegen_execution_method_returns_class_object() {
        let input = r#"
            class Point {
                int x;
                int y;
            }
            
            class PointFactory {
                Point createPoint(int x, int y) {
                    Point p = new Point();
                    p.x = x;
                    p.y = y;
                    return p;
                }
                
                Point createOrigin() {
                    return this.createPoint(0, 0);
                }
            }
            
            void main() {
                PointFactory factory = new PointFactory();
                Point p1 = factory.createPoint(10, 20);
                print(p1.x);
                print(p1.y);
                
                Point origin = factory.createOrigin();
                print(origin.x);
                print(origin.y);
                
                // Test chaining method calls
                Point p2 = factory.createPoint(5, 7);
                print(p2.x + p2.y); // Should print 12
            }
        "#;
        let result = compile_and_run(input);
        assert_eq!(result, "10200012"); // 10, 20, 0, 0, 12 (5+7)
    }

    fn compile_and_run(input: &str) -> String {
        use wasmtime::{Engine, Module, Store, Linker, Config};
        use std::sync::{Arc, Mutex};
        let ast = parse(input);
        let mut codegen = CodeGenerator::new(false);
        let wat = codegen.generate(ast);
        eprintln!("{}", &wat);
        let wasm = wat::Parser::default().generate_dwarf(wat::GenerateDwarf::Lines).parse_str(None, &wat).expect("Generated WAT should be valid");

        // Setup wasmtime
        let mut config = Config::new();
        config.wasm_multi_memory(true);
        config.wasm_gc(true);
        let engine = Engine::new(&config).unwrap();
        let module = Module::from_binary(&engine, &wasm).unwrap();
        let mut linker = Linker::new(&engine);
        let mut store = Store::new(&engine, ());

        // Capture print output
        let output = Arc::new(Mutex::new(String::new()));

        linker.func_wrap("host", "printInt", {
            let output = output.clone();
            move |val: i32| {
                let mut out = output.lock().unwrap();
                out.push_str(&val.to_string());
            }
        }).unwrap();
        linker.func_wrap("host", "printBool", {
            let output = output.clone();
            move |val: i32| {
                let mut out = output.lock().unwrap();
                out.push_str(&val.to_string());
            }
        }).unwrap();
        linker.func_wrap("host", "printFloat", {
            let output = output.clone();
            move |val: f32| {
                let mut out = output.lock().unwrap();
                out.push_str(&val.to_string());
            }
        }).unwrap();

        linker.func_wrap("host", "printString", {
            // This is a stub; actual string extraction from memory would be needed for real output.
            let output = output.clone();
            move |mut caller: Caller<'_, ()>, val: Rooted<ArrayRef>| {
                let mut out = output.lock().unwrap();
                let len = val.len(&caller).unwrap();
                for i in 0..len {
                    let val = val.get(&mut caller, i).unwrap();
                    let byte = val.i32().unwrap();
                    out.push(byte as u8 as char);
                }
            }
        }).unwrap();

        // Setup println functions for tests
        linker.func_wrap("host", "printlnInt", {
            let output = output.clone();
            move |val: i32| {
                let mut out = output.lock().unwrap();
                out.push_str(&val.to_string());
                out.push('\n');
            }
        }).unwrap();
        linker.func_wrap("host", "printlnBool", {
            let output = output.clone();
            move |val: i32| {
                let mut out = output.lock().unwrap();
                out.push_str(&val.to_string());
                out.push('\n');
            }
        }).unwrap();
        linker.func_wrap("host", "printlnFloat", {
            let output = output.clone();
            move |val: f32| {
                let mut out = output.lock().unwrap();
                out.push_str(&val.to_string());
                out.push('\n');
            }
        }).unwrap();
        linker.func_wrap("host", "printlnString", {
            let output = output.clone();
            move |mut caller: Caller<'_, ()>, val: Rooted<ArrayRef>| {
                let mut out = output.lock().unwrap();
                let len = val.len(&caller).unwrap();
                for i in 0..len {
                    let val = val.get(&mut caller, i).unwrap();
                    let byte = val.i32().unwrap();
                    out.push(byte as u8 as char);
                }
                out.push('\n');
            }
        }).unwrap();

        let instance = linker.instantiate(&mut store, &module).unwrap();
        let main = instance.get_func(&mut store, "main").expect("main function");
        main.call(&mut store, &[], &mut []).expect("main should run");

        output.lock().unwrap().clone()
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
        assert_eq!(result, "hi");
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

    #[test]
    fn codegen_execution_array_type_and_access() {
        let input = r#"
            void main() {
                int[] arr = new int[5];
                arr[0] = 42;
                int x = arr[0];
                print(x);
            }
        "#;
        let result = compile_and_run(input);
        assert_eq!(result, "42");
    }

    #[test]
    fn codegen_execution_array_of_objects() {
        let input = r#"
            class Point {
                int x;
                int y;
            }
            void main() {
                Point[] points = new Point[3];
                points[0] = new Point();
                points[0].x = 10;
                points[0].y = 20;
                points[1] = new Point();
                points[1].x = 5;
                points[1].y = 15;
                Point p = points[0];
                print(p.x + p.y + points[1].x + points[1].y); // Should print 50 (10+20+5+15)
            }
        "#;
        let result = compile_and_run(input);
        assert_eq!(result, "50");
    }

    #[test]
    fn codegen_execution_bitwise_operations() {
        let input = r#"
            void main() {
                int a = 12; // 1100 in binary
                int b = 10; // 1010 in binary
                
                // Bitwise AND: 1100 & 1010 = 1000 = 8
                print(a & b);
                
                // Bitwise OR: 1100 | 1010 = 1110 = 14
                print(a | b);
                
                // Bitwise XOR: 1100 ^ 1010 = 0110 = 6
                print(a ^ b);
                
                // Bitwise NOT: ~1100 = ...11110011 = -13 (two's complement)
                print(~a);
                
                // Left shift: 1100 << 2 = 110000 = 48
                print(a << 2);
                
                // Right shift: 1100 >> 1 = 0110 = 6
                print(a >> 1);
                
                // Unsigned right shift: same as signed for positive numbers
                print(a >>> 1);
            }
        "#;
        let result = compile_and_run(input);
        // Expected: 8, 14, 6, -13, 48, 6, 6
        assert_eq!(result, "8146-134866");
    }

    #[test]
    fn codegen_execution_bitwise_compound_assignments() {
        let input = r#"
            void main() {
                int x = 15; // 1111 in binary
                
                // x &= 7  =>  x = x & 7  =>  1111 & 0111 = 0111 = 7
                x &= 7;
                print(x);
                
                // x |= 8  =>  x = x | 8  =>  0111 | 1000 = 1111 = 15
                x |= 8;
                print(x);
                
                // x ^= 3  =>  x = x ^ 3  =>  1111 ^ 0011 = 1100 = 12
                x ^= 3;
                print(x);
            }
        "#;
        let result = compile_and_run(input);
        // Expected: 7, 15, 12
        assert_eq!(result, "71512");
    }

    #[test]
    fn codegen_execution_println() {
        let input = r#"
            void main() {
                println(42);
                println("hello");
                println(true);
            }
        "#;
        let result = compile_and_run(input);
        // Expected: "42\nhello\n1\n"  
        assert_eq!(result, "42\nhello\n1\n");
    }

    #[test]
    fn codegen_execution_simple_recursive() {
        let input = r#"
            int simple(int n) {
                if (n <= 0) {
                    return 1;
                } else {
                    return n + simple(n - 1);
                }
            }
            
            void main() {
                print(simple(3));
            }
        "#;
        let result = compile_and_run(input);
        // Expected: 1 + 2 + 3 + 1 = 7
        assert_eq!(result, "7");
    }

    #[test]
    fn codegen_execution_single_branch_return() {
        let input = r#"
            int test(int x) {
                if (x > 0) {
                    return x;
                }
                return 0;
            }
            
            void main() {
                print(test(5));
                print(test(-3));
            }
        "#;
        let result = compile_and_run(input);
        // Expected: test(5)=5, test(-3)=0
        assert_eq!(result, "50");
    }

    #[test]
    #[should_panic(expected = "Compile error: Function 'test' with return type 'int' does not have a return statement on all code paths")]
    fn codegen_execution_single_branch_return_no_fallback() {
        let input = r#"
            int test(int x) {
                if (x > 0) {
                    return x;
                }
                // No else branch, no return after - this should be a compile error
            }
            
            void main() {
                print(test(5));
            }
        "#;
        let _result = compile_and_run(input);
    }

    #[test]
    #[should_panic(expected = "Compile error: Function 'test' with return type 'int' does not have a return statement on all code paths")]
    fn codegen_execution_else_only_return() {
        let input = r#"
            int test(int x) {
                if (x > 0) {
                    // No return in then branch
                } else {
                    return 0;
                }
                // No return after - this should be a compile error
            }
            
            void main() {
                print(test(-1));
            }
        "#;
        let _result = compile_and_run(input);
    }

    #[test]
    fn codegen_execution_proper_return_coverage() {
        let input = r#"
            int test(int x) {
                if (x > 0) {
                    return x * 2;
                } else {
                    return 0;
                }
                // Both branches return - this is correct
            }
            
            void main() {
                print(test(5));
                print(test(-3));
            }
        "#;
        let result = compile_and_run(input);
        assert_eq!(result, "100");
    }

    #[test]
    fn codegen_execution_recursive_functions() {
        let input = r#"
            int factorial(int n) {
                if (n <= 1) {
                    return 1;
                } else {
                    return n * factorial(n - 1);
                }
            }
            
            int fibonacci(int n) {
                if (n <= 1) {
                    return n;
                } else {
                    return fibonacci(n - 1) + fibonacci(n - 2);
                }
            }
            
            void main() {
                // Test factorial: 5! = 120
                print(factorial(5));
                
                // Test fibonacci: fib(6) = 8
                print(fibonacci(6));
                
                // Test recursive function with larger values
                print(factorial(6)); // 6! = 720
                
                // Test edge cases
                print(factorial(0)); // 0! = 1
                print(factorial(1)); // 1! = 1
                print(fibonacci(0)); // fib(0) = 0
                print(fibonacci(1)); // fib(1) = 1
            }
        "#;
        let result = compile_and_run(input);
        // Expected: factorial(5)=120, fibonacci(6)=8, factorial(6)=720, factorial(0)=1, factorial(1)=1, fibonacci(0)=0, fibonacci(1)=1
        assert_eq!(result, "12087201101");
    }

    #[test]
    fn codegen_execution_null_literal() {
        let input = r#"
            void main() {
                String s = null;
                boolean isNull = (s == null);
                print(isNull);
            }
        "#;
        let result = compile_and_run(input);
        assert_eq!(result, "1"); // true, since s is null
    }

    #[test]
    fn codegen_execution_function_returns_class_object() {
        let input = r#"
            class Point {
                int x;
                int y;
            }
            
            Point createPoint(int x, int y) {
                Point p = new Point();
                p.x = x;
                p.y = y;
                return p;
            }
            
            void main() {
                Point p = createPoint(10, 20);
                print(p.x);
                print(p.y);
                
                // Test with different values
                Point p2 = createPoint(5, 15);
                print(p2.x + p2.y); // Should print 20
            }
        "#;
        let result = compile_and_run(input);
        assert_eq!(result, "102020"); // 10, 20, 20 (5+15)
    }


    #[test]
    fn codegen_execution_null_object_fields() {
        let input = r#"
            class Node {
                int value;
                Node next;
            }
            
            void main() {
                Node n = new Node();
                n.value = 42;
                n.next = null;
                
                // Test field is null
                print(n.next == null); // Should print 1 (true)
                print(n.next != null); // Should print 0 (false)
                
                // Test assigning object to field, then null
                Node n2 = new Node();
                n2.value = 10;
                n.next = n2;
                print(n.next == null); // Should print 0 (false)
                
                n.next = null;
                print(n.next == null); // Should print 1 (true)
            }
        "#;
        let result = compile_and_run(input);
        assert_eq!(result, "1001"); // 1, 0, 0, 1
    }

        #[test]
    fn codegen_execution_null_method_parameters() {
        let input = r#"
            class Container {
                String data;
                
                void setData(String s) {
                    this.data = s;
                }
                
                boolean isDataNull() {
                    return this.data == null;
                }
            }
            
            void main() {
                Container c = new Container();
                
                // Initially data should be null (default)
                print(c.isDataNull()); // Should print 1 (true)
                
                // Pass null explicitly
                c.setData(null);
                print(c.isDataNull()); // Should print 1 (true)
                
                // Set to a string
                c.setData("hello");
                print(c.isDataNull()); // Should print 0 (false)
                
                // Set back to null
                c.setData(null);
                print(c.isDataNull()); // Should print 1 (true)
            }
        "#;
        let result = compile_and_run(input);
        assert_eq!(result, "1101"); // 1, 1, 0, 1
    }

    #[test]
    fn codegen_execution_null_function_returns() {
        let input = r#"
            class Person {
                int value;
            }
            
            Person createPersonOrNull(boolean shouldCreate) {
                if (shouldCreate) {
                    return new Person();
                } else {
                    return null;
                }
            }
            
            void main() {
                // Function returns object
                Person p1 = createPersonOrNull(true);
                print(p1.value);
            }
        "#;
        let result = compile_and_run(input);
        assert_eq!(result, "0"); // Default value 0
    }
}
