mod lexer;
mod parser;
use crate::lexer::Lexer;
use crate::parser::*;

use std::fs;

fn main() {
    // Sample input string in your mini-language.
    let input_code = r#"
        let x = 5;
        let y = 10;
        if (x == y) { 
            x = x + y 
        } else {
            let z = 0;
            while (z != 5) {
                x = x + 1;
            };
        };
    "#;

    // 1. Lexing the input.
    let lexer = Lexer::new(input_code);
    let tokens = lexer.lex();

    // 2. Parsing the tokens into an AST.
    let mut parser = Parser::new(tokens);
    match parser.parse() {
        Ok(ast) => {
            // 3. Convert the AST to a Bash script.
            let bash_code = ast.to_bash();

            // For the sake of this example, print the Bash script.
            println!("Generated Bash Code:\n{}", bash_code);

            // Optionally, save the Bash script to a file.
            fs::write("output_script.sh", bash_code).expect("Unable to write to file");
        }
        Err(e) => println!("Error: {}", e),
    }
}
