#  Tiny Language Parser

A recursive descent parser for a minimal programming language called "Tiny Language", written in C++.  
Supports tokenization, syntax tree construction, and rich error diagnostics.

## ✨ Features

- Tokenizes input code into structured tokens.
- Parses the tokens using a recursive descent parser.
- Builds an Abstract Syntax Tree (AST).
- Provides detailed syntax error messages with suggestions.
- Implements error recovery to continue parsing after errors.
- Logs internal parsing steps for debugging or teaching purposes.

## 🧠 Supported Grammar

The Tiny Language supports:
- Variable assignment (`:=`)
- Input/Output (`read`, `write`)
- Conditional statements (`if ... then ... end`)
- Looping (`repeat ... until ...`)
- Arithmetic expressions (`+`, `-`, `*`)
- Comparisons (`=`, `<`)

## 🛠️ Components

### 1. **Tokenizer**
Converts raw text into tokens like:
- `IDENTIFIER`
- `NUMBER`
- `KEYWORD`
- `OPERATOR`
- `PUNCTUATION`
- `END_OF_FILE`
- `INVALID`

### 2. **Parser**
Implements grammar rules recursively:
- `parseProgram()`
- `parseStatementSequence()`
- `parseStatement()`
- `parseAssignStatement()`
- `parseIfStatement()`
- `parseRepeatStatement()`
- `parseExpression()`, `parseTerm()`, `parseFactor()`
- `parseComparison()`

### 3. **Abstract Syntax Tree (AST)**
Represents parsed program logic in a hierarchical structure.

### 4. **Error Handling**
Provides detailed error messages:
- Type of error
- Line number
- What was found
- What was expected
- Suggestion for fix

Example:

https://github.com/user-attachments/assets/30c60980-e020-4d4c-9c39-dde28746b982

