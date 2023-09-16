//  ____                                     ____                      _ _
// | __ )  ___  _   _ _ __ _ __   ___       / ___|___  _ __ ___  _ __ (_) | ___
// |  _ \ / _ \| | | | '__| '_ \ / _ \_____| |   / _ \| '_ ` _ \| '_ \| | |/ _ \
// | |_) | (_) | |_| | |  | | | |  __/_____| |__| (_) | | | | | | |_) | | |  __/
// |____/ \___/ \__,_|_|  |_| |_|\___|      \____\___/|_| |_| |_| .__/|_|_|\___|
//                                                              |_|

//! # Compiler to Bash
//!
//! This module serves as the main entry point for the compiler. It handles command line arguments,
//! reading the source file, tokenizing, parsing, and generating the Bash output.
//!
//! ## Usage
//!
//! To use this compiler, you should specify a source file as an argument. The compiled Bash script
//! will be outputted to the appropriate directory or file.
//!
// Path: src/main.rs

#![recursion_limit = "256"]

// Enables the AST visualizer when debugging is enabled.
#[cfg(debug_assertions)]
mod ast_visualizer;

// Modules responsible for lexing and parsing the input.
mod lexer;
mod parser;

use crate::lexer::Lexer;
use crate::parser::*;
use clap::Parser as ClapParser;
use std::fs;
use std::path::Path;
use std::process::Command;

/// Command line arguments structure.
#[derive(ClapParser, Debug, Clone)]
#[command(author, version, about, name = "Compiler to bash")]
struct Args {
    /// Path to the source file to be compiled.
    path: Option<String>,
}

/// Entry point for the compiler.
///
/// # Development mode
/// To run the compiler in development mode, use:
/// ```bash
/// cargo run -- examples/1.txt
/// ```
/// # Panics
/// This function will panic if there's a problem reading the file, writing the output, or
/// managing directories.
fn main() {
    let args = Args::parse();
    dbg!(&args);
    match args.path {
        Some(path) => {
            let path = Path::new(&path);
            if path.exists() && path.is_file() {
                let content = fs::read_to_string(path).expect("Unable to read file");
                let lexer = Lexer::new(&content);
                let tokens = lexer.lex();
                dbg!(&tokens);
                let mut parser = Parser::new(tokens);
                match parser.parse() {
                    Ok(ast) => {
                        // Generate the Bash code from the AST.
                        let mut bash_code = "#!/bin/bash\n\n".to_string();
                        bash_code += &ast.to_bash();

                        // Determine where to write the Bash output based on compile mode.
                        #[cfg(debug_assertions)]
                        {
                            // Write to a file in the build directory when in debug mode.
                            let build_path = std::path::Path::new("build/dev");
                            if build_path.exists() {
                                fs::remove_dir_all(build_path).expect("Unable to remove directory");
                                fs::create_dir_all(build_path).expect("Unable to create directory");
                            } else {
                                fs::create_dir_all(build_path).expect("Unable to create directory");
                            }

                            let dot = ast_visualizer::visualize_ast(&ast);

                            fs::write("build/dev/output_ast.dot", dot)
                                .expect("Unable to write to file");

                            fs::write("build/dev/output_script.bash", bash_code)
                                .expect("Unable to write to file");
                            Command::new("chmod")
                                .arg("+x")
                                .arg("build/dev/output_script.bash")
                                .output()
                                .expect("Unable to change permissions");
                            Command::new("dot")
                                .arg("-Tpng")
                                .arg("build/dev/output_ast.dot")
                                .arg("-o")
                                .arg("build/dev/output_ast.png")
                                .output()
                                .expect("Unable to generate AST image");
                        }
                        #[cfg(not(debug_assertions))]
                        {
                            fs::write("output_script.sh", bash_code)
                                .expect("Unable to write to file");
                            Command::new("chmod")
                                .arg("+x")
                                .arg("output_script.sh")
                                .output()
                                .expect("Unable to change permissions");
                        }
                    }
                    Err(e) => {
                        // Handle parsing errors.
                        println!("Failed to parse input code.");
                        println!("Code: {}", content);
                        println!("Error: {}", e);
                    }
                }
            }
        }
        _ => {
            // Handle incorrect arguments.
            println!("Incorrect args");
        }
    }
}
