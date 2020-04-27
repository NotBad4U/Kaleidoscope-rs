#![feature(proc_macro_hygiene)]
extern crate llvm_sys;
extern crate plex;

use clap::Clap;
use inkwell::context::Context;
use inkwell::passes::PassManager;
use inkwell::OptimizationLevel;
use libc::{c_char, c_void};

use std::error::Error;
use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::io::Write;

mod ircodegen;
mod lexer;
mod parser;

use parser::ASTNode;

#[derive(Clap)]
#[clap(version = "1.0")]
struct Opts {
    #[clap(help = "path of source file")]
    source_file: Option<String>,
    #[clap(short = "l", long = "display-lexer")]
    display_lexer_output: bool,
    #[clap(short = "p", long = "display-parser")]
    display_parser_output: bool,
    #[clap(short = "c", long = "display-ircodegen")]
    display_compiler_output: bool,
    #[clap(short = "o", long = "show-optimization")]
    show_optimization: bool,
}

macro_rules! print_flush {
    ( $( $x:expr ),* ) => {
        print!( $($x, )* );

        std::io::stdout().flush().expect("Could not flush to standard output.");
    };
}

#[no_mangle]
pub extern "C" fn putchard(x: f64) -> f64 {
    print_flush!("{}", x as u8 as char);
    x
}

#[no_mangle]
pub extern "C" fn printd(x: f64) -> f64 {
    println!("{}", x);
    x
}

pub fn init_extern_function() {
    unsafe {
        add_symbol("printd", printd as *const ());
        add_symbol("putchard", putchard as *const ());
    }
}

pub unsafe fn add_symbol(name: &str, ptr: *const ()) {
    let name = std::ffi::CString::new(name).unwrap();
    let addr = ptr as *mut c_void;
    llvm_sys::support::LLVMAddSymbol(name.as_ptr() as *const c_char, addr)
}

// Execute node without arguments to start a REPL.
fn run_repl(opts: &Opts) -> Result<(), Box<dyn Error>> {
    let context = Context::create();
    let module = context.create_module("repl");
    let builder = context.create_builder();
    let pass_manager = PassManager::create(&module);

    pass_manager.add_instruction_combining_pass();
    pass_manager.add_reassociate_pass();
    pass_manager.add_gvn_pass();
    pass_manager.add_cfg_simplification_pass();
    pass_manager.add_promote_memory_to_register_pass(); // Promote allocas (mut variables) to registers.

    pass_manager.initialize();

    let ee = module.create_interpreter_execution_engine()?;

    init_extern_function();

    let mut codegen = ircodegen::CodeGen::new(
        &context,
        builder,
        module,
        Some(pass_manager),
        ircodegen::OptionCodegen::new(opts.display_compiler_output, opts.show_optimization),
    );

    // Generate code for external symbols
    codegen.gencode_prototype(&parser::Prototype {
        name: "printd".to_owned(),
        args: vec!["x".to_owned()],
    })?;
    codegen.gencode_prototype(&parser::Prototype {
        name: "putchard".to_owned(),
        args: vec!["x".to_owned()],
    })?;

    println!("Enter exit|quite or press ^C to abort current expression to exit the repl");

    loop {
        print_flush!("> ");

        let mut input = String::new();
        io::stdin().read_line(&mut input)?;

        if input.starts_with("exit") || input.starts_with("quit") {
            return Ok(());
        } else if input.chars().all(char::is_whitespace) {
            continue;
        }

        let mut lexer = lexer::Lexer::new(&input);
        let tokens = lexer.tokenize();

        if opts.display_lexer_output {
            println!("tokens = {:?}", tokens);
        }

        let progam_ast = parser::parse(tokens.into_iter()).expect("error parser");

        if opts.display_parser_output {
            println!("ast = {:?}", progam_ast.stmts);
        }

        let func = match &progam_ast.stmts[0] {
            ASTNode::Function(f) => codegen.gencode_function(&f),
            ASTNode::Extern(p) => codegen.gencode_prototype(&p),
            ASTNode::Expression(e) => {
                codegen.gencode_function(&parser::create_toplevel_expr(e.clone()))
            }
        };

        match func {
            Ok(func) => {
                match &progam_ast.stmts[0] {
                    ASTNode::Expression(_) => {
                        let res = unsafe {
                            ee.run_function(
                                func,
                                &[], //anonymous function, so we don't need it
                            )
                        };

                        println!("=> {:?}", res.as_float(&context.f64_type()));
                    }
                    _ => {}
                };
            }
            Err(e) => eprintln!("ir-error = {}", e),
        }

        println!("");
    }
}

fn run_jit_engine(opts: &Opts) -> Result<(), Box<dyn Error>> {
    let mut file = File::open(opts.source_file.as_ref().unwrap())?;
    let mut contents = String::new();

    file.read_to_string(&mut contents)?;

    let mut lexer = lexer::Lexer::new(&contents);
    let tokens = lexer.tokenize();

    if opts.display_lexer_output {
        println!("tokens = {:?}", tokens);
    }

    let progam_ast = parser::parse(tokens.into_iter()).expect("error parser");

    if opts.display_parser_output {
        println!("ast = {:?}", progam_ast.stmts);
    }

    let context = Context::create();
    let module = context.create_module("main");
    let builder = context.create_builder();
    let pass_manager = PassManager::create(&module);

    pass_manager.add_instruction_combining_pass();
    pass_manager.add_reassociate_pass();
    pass_manager.add_gvn_pass();
    pass_manager.add_cfg_simplification_pass();
    pass_manager.add_promote_memory_to_register_pass(); // Promote allocas (mut variables) to registers.

    pass_manager.initialize();

    let jit = module.create_jit_execution_engine(OptimizationLevel::None)?;

    init_extern_function();

    let mut codegen = ircodegen::CodeGen::new(
        &context,
        builder,
        module,
        Some(pass_manager),
        ircodegen::OptionCodegen::new(opts.display_compiler_output, opts.show_optimization),
    );

    // Generate code for external symbols
    codegen.gencode_prototype(&parser::Prototype {
        name: "printd".to_owned(),
        args: vec!["x".to_owned()],
    })?;
    codegen.gencode_prototype(&parser::Prototype {
        name: "putchard".to_owned(),
        args: vec!["x".to_owned()],
    })?;

    let res = codegen.gencode_program(progam_ast)?;

    let main = jit.get_function_value("main")?;

    let res = unsafe { jit.run_function(main, &[]) };

    Ok(())
}

fn main() -> Result<(), Box<dyn Error>> {
    let opts: Opts = Opts::parse();

    if opts.source_file.is_some() {
        run_jit_engine(&opts)?;
    } else {
        run_repl(&opts)?;
    }

    Ok(())
}
