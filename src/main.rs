// Code for the main function of the compiler.
// Path: src/main.rs

#[cfg(debug_assertions)]
mod ast_visualizer;

mod lexer;
mod parser;

use crate::lexer::Lexer;
use crate::parser::*;
use clap::Parser as ClapParser;
use std::fs;
use std::path::Path;

#[derive(ClapParser, Debug, Clone)]
#[command(author, version, about, name = "Compiler to bash")]
struct Args {
    path: Option<String>,
}

fn main() {
    let args = Args::parse();

    match args.path {
        Some(path) => {
            let path = Path::new(&path);
            if path.exists() && path.is_file() {
                let content = fs::read_to_string(path).expect("Unable to read file");
                let lexer = Lexer::new(&content);
                let tokens = lexer.lex();
                let mut parser = Parser::new(tokens);
                match parser.parse() {
                    Ok(ast) => {
                        #[cfg(debug_assertions)]
                        {
                            let ast_visualization = ast_visualizer::visualize_ast(&ast);
                            let build_path = std::path::Path::new("build/dev");
                            if build_path.exists() {
                                fs::remove_dir_all(build_path).expect("Unable to remove directory");
                                fs::create_dir_all(build_path).expect("Unable to create directory");
                            } else {
                                fs::create_dir_all(build_path).expect("Unable to create directory");
                            }
                            fs::write("build/dev/ast.dot", ast_visualization)
                                .expect("Unable to write to file");
                        }

                        let mut bash_code = "#!/bin/bash\n\n".to_string();
                        bash_code += &ast.to_bash();

                        #[cfg(debug_assertions)]
                        fs::write("build/dev/dev.sh", bash_code).expect("Unable to write to file");
                        #[cfg(not(debug_assertions))]
                        {
                            let name = path.file_name().unwrap().to_str().unwrap();
                            dbg!(name);
                            fs::write("output_script.sh", bash_code)
                                .expect("Unable to write to file");
                        }
                    }
                    Err(e) => {
                        println!("Failed to parse input code.");
                        println!("Code: {}", content);
                        println!("Error: {}", e);
                    }
                }
            }
        }
        _ => {
            println!("Incorrect args")
        }
    }
}
