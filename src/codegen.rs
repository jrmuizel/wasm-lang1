// codegen.rs

use crate::parser::{Stmt, Expr, Token};
use std::collections::{HashMap, HashSet};
use std::cell::RefCell;

pub struct CodeGenerator {
    classes: HashMap<String, Vec<(String, String)>>,
    output: RefCell<String>,
    indent_level: usize,
    local_vars_stack: Vec<HashSet<String>>, // Stack of local variable names
}

impl CodeGenerator {
    pub fn new() -> Self {
        CodeGenerator {
            classes: HashMap::new(),
            output: RefCell::new(String::new()),
            indent_level: 0,
            local_vars_stack: Vec::new(),
        }
    }

    pub fn generate(&mut self, stmts: Vec<Stmt>) -> String {
        self.output.borrow_mut().push_str("(module\n");
        self.indent_level += 1;

        // Import print functions
        self.emit_line("(import \"host\" \"printInt\" (func $printInt (param i32)))");
        self.emit_line("(import \"host\" \"printBool\" (func $printBool (param i32)))");
        self.emit_line("(import \"host\" \"printString\" (func $printString (param (ref $String))))");
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
                let wasm_type = match field_type.as_str() {
                    "int" => "i32",
                    "boolean" => "i32",
                    "String" => "(ref $String)",
                    _ => "(ref $Object)" // Default to generic object
                };
                self.emit(&format!("\n  (field ${} {})", field_name, wasm_type));
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
                    self.emit(&format!("(global ${} (mut {})", name, self.type_to_wasm(&var_type)));
                    if let Some(expr) = init {
                        let value = self.generate_expr(&expr);
                        self.emit(&format!("(global.set ${} {})\n", name, value));
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
        let mut locals_set = std::collections::HashSet::new();
        for (param_type, param_name) in params.iter() {
            self.emit(&format!(
                " (param ${} {})",
                param_name,
                self.type_to_wasm(param_type)
            ));
            locals_set.insert(param_name.clone());
        }
        self.emit(&format!(
            " (result {})\n",
            self.type_to_wasm(return_type)
        ));
        // Emit local variable declarations
        let locals = self.collect_locals(body, &locals_set);
        for (name, var_type) in &locals {
            self.emit(&format!(" (local ${} {})", name, self.type_to_wasm(var_type)));
            locals_set.insert(name.clone());
        }
        self.indent_level += 1;
        self.local_vars_stack.push(locals_set);
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
        self.emit(&format!("(func ${}.{}", class_name, method_name));
        self.emit(&format!(" (param $this (ref ${}))", class_name));
        let mut locals_set = std::collections::HashSet::new();
        locals_set.insert("this".to_string());
        for (param_type, param_name) in params.iter() {
            self.emit(&format!(
                " (param ${} {})",
                param_name,
                self.type_to_wasm(param_type)
            ));
            locals_set.insert(param_name.clone());
        }
        self.emit(&format!(
            " (result {})\n",
            self.type_to_wasm(return_type)
        ));
        // Emit local variable declarations
        let locals = self.collect_locals(body, &locals_set);
        for (name, var_type) in &locals {
            self.emit(&format!(" (local ${} {})", name, self.type_to_wasm(var_type)));
            locals_set.insert(name.clone());
        }
        self.indent_level += 1;
        self.local_vars_stack.push(locals_set);
        self.generate_stmt(body);
        if return_type == "void" {
            self.emit_line("return");
        }
        self.local_vars_stack.pop();
        self.indent_level -= 1;
        self.emit_line(")");
    }

    fn generate_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Block(stmts) => {
                // New scope for block
                self.local_vars_stack.push(HashSet::new());
                for s in stmts {
                    // If it's a variable declaration, add to locals
                    if let Stmt::VariableDecl { name, .. } = s {
                        if let Some(locals) = self.local_vars_stack.last_mut() {
                            locals.insert(name.clone());
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
                    self.emit(&format!("(local.set ${} {})\n", name, value));
                }
            }
            Stmt::Assignment { name, value } => {
                let expr_value = self.generate_expr(value);
                self.emit(&format!("(global.set ${} {})\n", name, expr_value));
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
                self.emit(&format!("(br_if 1 (i32.eqz {}))\n", cond));
                self.generate_stmt(body);
                self.emit_line("(br 0)");
                self.emit_line(")");
                self.emit_line(")");
            }
            Stmt::Print(expr) => {
                let value = self.generate_expr(expr);
                self.emit(&format!("(call $print {})\n", value));
            }
            Stmt::Return { value } => {
                if let Some(expr) = value {
                    let ret_value = self.generate_expr(expr);
                    self.emit(&format!("return {}\n", ret_value));
                } else {
                    self.emit_line("return");
                }
            }
            Stmt::Expression(expr) => {
                let value = self.generate_expr(expr);
                self.emit(&format!("{}\n", value));
            }
            _ => {}
        }
    }

    fn generate_expr(&mut self, expr: &Expr) -> String {
        match expr {
            Expr::LiteralInt(n) => format!("i32.const {}", n),
            Expr::LiteralBool(b) => format!("i32.const {}", if *b { 1 } else { 0 }),
            Expr::LiteralString(s) => {
                self.emit(&format!("(array.new $String (i32.const {})", s.len()));
                for (i, c) in s.chars().enumerate() {
                    self.emit(&format!("(array.set $String (i32.const {}) (i32.const {}))", i, c as u32));
                }
                "(ref $String)".to_string()
            }
            Expr::Variable(name) => {
                // Check if name is a local variable
                if self.local_vars_stack.iter().rev().any(|locals| locals.contains(name)) {
                    format!("local.get ${}", name)
                } else {
                    format!("global.get ${}", name)
                }
            }
            Expr::BinaryOp { op, left, right } => {
                let left_expr = self.generate_expr(left);
                let right_expr = self.generate_expr(right);
                match op.as_str() {
                    "+" => format!("i32.add {} {}", left_expr, right_expr),
                    "-" => format!("i32.sub {} {}", left_expr, right_expr),
                    "*" => format!("i32.mul {} {}", left_expr, right_expr),
                    "/" => format!("i32.div_s {} {}", left_expr, right_expr),
                    "==" => format!("i32.eq {} {}", left_expr, right_expr),
                    "!=" => format!("i32.ne {} {}", left_expr, right_expr),
                    "<" => format!("i32.lt_s {} {}", left_expr, right_expr),
                    ">" => format!("i32.gt_s {} {}", left_expr, right_expr),
                    "<=" => format!("i32.le_s {} {}", left_expr, right_expr),
                    ">=" => format!("i32.ge_s {} {}", left_expr, right_expr),
                    "&&" => format!("i32.and {} {}", left_expr, right_expr),
                    "||" => format!("i32.or {} {}", left_expr, right_expr),
                    _ => panic!("Unsupported operator: {}", op),
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
                format!("struct.get ${} ${} {}", self.get_class_name(object), field, obj)
            }
            Expr::MethodCall { object, method, args } => {
                let obj = self.generate_expr(object);
                let mut call_args = String::new();
                for arg in args {
                    call_args.push_str(&format!("{} ", self.generate_expr(arg)));
                }
                format!("call ${}.{} {} {}", self.get_class_name(object), method, obj, call_args)
            }
            Expr::New { class_name, args } => {
                let mut init_args = String::new();
                for arg in args {
                    init_args.push_str(&format!("{} ", self.generate_expr(arg)));
                }
                format!("struct.new ${} {}", class_name, init_args)
            }
            Expr::This => "local.get $this".to_string(),
            Expr::FunctionCall { name, args } => {
                let mut call_args = String::new();
                for arg in args {
                    call_args.push_str(&format!("{} ", self.generate_expr(arg)));
                }
                format!("(call ${} {})", name, call_args)
            },
            _ => "".to_string(),
        }
    }

    fn get_class_name(&self, expr: &Expr) -> String {
        // Simplified: extract class name from expression
        match expr {
            Expr::Variable(name) => name.clone(),
            _ => "Object".to_string(),
        }
    }

    fn type_to_wasm(&self, t: &str) -> &str {
        match t {
            "int" => "i32",
            "boolean" => "i32",
            "String" => "(ref $String)",
            "void" => "",
            _ => "(ref $Object)",
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
}