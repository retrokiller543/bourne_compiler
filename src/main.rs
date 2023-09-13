mod lexer;
mod parser;
use crate::lexer::Lexer;
use crate::parser::*;

use std::fs;

fn main() {
    // Sample input string in your mini-language.
    let input_code = r#"
        let x = 10;
        let y = 20;
        let z = x + y;
        print("Sum of x and y is:");
        print(z);
        
        if z == 30
        {
            print("z is 30");
        }
        else
        {
            print("z is not 30");
        };
        
        while x != 0 
        {
            print("Counting down: ");
            print(x);
            x = x - 1;
        };
        
        print("Done counting!");
    "#;

    // 1. Lexing the input.
    let lexer = Lexer::new(input_code);
    let tokens = lexer.lex();

    // 2. Parsing the tokens into an AST.
    let mut parser = Parser::new(tokens);
    match parser.parse() {
        Ok(ast) => {
            // 3. Convert the AST to a Bash script.
            let mut bash_code = "#!/bin/bash\n".to_string(); // Add shebang here
            bash_code += &ast.to_bash();

            // Optionally, save the Bash script to a file.
            fs::write("output_script.sh", bash_code).expect("Unable to write to file");
        }
        Err(e) => {
            println!("Failed to parse input code.");
            println!("Code: {}", input_code);
            println!("Error: {}", e);
        },
    }
}
