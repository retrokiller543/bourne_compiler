
# README

## Overview

Welcome to the Bash-like compiler! This project is designed to simplify the process of converting Rust-like syntax to bash scripts. It's a great way to easily transition from a high-level language like Rust to writing bash scripts.

## Structure

The compiler primarily consists of three modules:

1. **Lexer** (`src/lexer/mod.rs`): Responsible for breaking the input string into recognizable tokens.
2. **Parser** (`src/parser/mod.rs`): Parses the tokens from the lexer into an Abstract Syntax Tree (AST) and provides a method to convert the AST to a bash script.
3. **Main** (`src/main.rs`): Uses the lexer and parser to convert a given input string to a bash script.

## Grammar

The syntax of the mini-language designed here is similar to Rust and is described as follows:

```m
program     => statement*

statement   => declaration
            | function_call
            | if_statement
            | while_loop
            | expression EoL

declaration => "let" Identifier ("=" expression)?

function_call => BuiltInFunction "(" arguments? ")"
              | Identifier "(" arguments? ")"

if_statement  => "if" expression "{" statement* "}"
                ("else" "{" statement* "}")?

while_loop   => "while" expression "{" statement* "}"

expression  => term (("+" | "-") term)*

term        => factor (("*" | "/") factor)*

factor      => "(" expression ")"
            | "-" factor
            | Number
            | StringLiteral
            | function_call
            | Identifier

arguments   => expression ("," expression)*
```

### Tokens

The lexer breaks down the input into the following tokens:

- **Number**: Represents a 32-bit integer.
- **StringLiteral**: Represents a string.
- **Operator**: Mathematical and comparison operators.
- **Identifier**: Variable or function names.
- **Keyword**: Reserved words such as `if`, `else`, `while`, etc.
- **BuiltInFunction**: Functions provided out-of-the-box, like `print`, `arg`, `exec`, and `exit`.
- **Punctuation**: Symbols like `(`, `)`, `{`, `}`, and `;`.

### Keywords

The recognized keywords include:

- `if`
- `else`
- `elseif`
- `while`
- `for`
- `let`

### Built-in Functions

The following built-in functions are available:

- `print`: Print to the console.
- `arg`: Get an argument passed to the script.
- `exec`: Execute a command.
- `exit`: Exit the script.

## Example

Consider the following code snippet in our mini-language:

```rust
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

print("Counting down: ");

while x != 0
{
    print(x);
    x = x - 1;
};

print("Done counting!");
```

This code declares three variables (`x`, `y`, and `z`), prints their sum, checks if `z` is equal to 30, and counts down from `x` to 0.

## Using the Compiler

To convert your code in our mini-language to a bash script, follow these steps:

1. Place your code inside the `input_code` string in the `main` function located in `src/main.rs`.
2. Run the compiler.
3. The output will be saved to `output_script.sh`.

You can then execute the bash script by running:

```bash
chmod +x output_script.sh
./output_script.sh
```

# Side Notes

- The compiler is not designed to handle syntax errors in all cases yet. If you encounter an error, please check your code for syntax errors or feel free to add a pull request to fix the issue.

- The compiler is very much early stages and is not designed to handle all cases and has a bunch of things that dont work as you might think. If you encounter an error, please check your code for syntax errors or feel free to add a pull request to fix the issue.

- This is a hobby project and is not designed to be used in production and mostly designed to make it simpler to write bash scripts for begginers.

---

# Conclusion

Can currently handle the following:
- [x] Arithmetic Expressions
- [x] Variables
- [x] Built in Functions
- [x] If Statements
- [x] Else Statements
- [ ] Else If Statements
- [x] While Loops
- [ ] Functions (WIP)
- [ ] For Loops (WIP)

## Contributing

If you would like to contribute to this project, please feel free to fork the repository and add a pull request. I'm are always looking for ways to improve the compiler and make it more robust.

---

I hope this README provides a clear understanding of the compiler and how to use it. Happy coding!
