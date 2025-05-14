#pragma once

#include <iostream>
#include <string>
#include <vector>
#include <cctype>       // For isspace, isalpha, isalnum, isdigit
#include <stdexcept>    // Keep for unexpected C++ issues
#include <unordered_set> // For keyword lookup
#include <set>          // For synchronizing token sets
#include <utility>      // For std::move
#include <algorithm>    // For std::remove
#include <sstream>      // For std::stringstream (used internally for some helpers/debugging)


// --- Detailed Error Handling Structure ---

// Enum to categorize different types of syntax errors encountered during parsing.
enum class ErrorType {
    MISSING_SEMICOLON,      // Expected ';' but found something else or EOF.
    MISSING_KEYWORD,        // Expected a specific keyword (e.g., 'then', 'else', 'until', 'end').
    MISSING_IDENTIFIER,     // Expected an identifier (variable name).
    MISSING_OPERATOR,       // Expected a specific operator (e.g., ':=', '=', '>').
    MISSING_EXPRESSION,     // Expected a full expression (e.g., after ':=', 'write', comparison operator).
    MISSING_TERM,           // Expected a term/factor within an expression (identifier or number).
    MISSING_COMPARISON,     // Expected a full comparison (e.g., after 'if', 'until').
    UNEXPECTED_TOKEN,       // Found a token that doesn't fit the grammar rules at the current position.
    INVALID_TOKEN,          // Token representing a lexical error (e.g., unterminated comment, illegal character).
    UNEXPECTED_END,         // Reached the end of the input unexpectedly (e.g., missing 'end', 'until', statement).
    IDENTIFIER_NOT_ASSIGNMENT,// Found an identifier at the start of a statement not followed by ':='.
    INTERNAL_ERROR          // For C++ exceptions or unexpected internal parser states.
};

// Helper function to convert ErrorType enum to a human-readable string.
std::string errorTypeToString(ErrorType t);

// Structure to hold detailed information about a syntax error.
struct DetailedSyntaxError {
    std::string message;    // Generated descriptive message explaining the error.
    int line;               // Line number in the source code where the error occurred.
    ErrorType type;         // Categorized error type using the ErrorType enum.
    std::string foundValue; // The actual token value found at the error location (if applicable).
    std::string foundType;  // The actual token type found (as a string).
    std::string expectedDesc; // Description of what the parser expected (e.g., ";", "keyword 'then'", "identifier").
    std::string suggestion; // A possible suggestion for how to fix the error.

    // Constructor declaration
    DetailedSyntaxError(std::string msg, int l, ErrorType t, std::string fVal = "", std::string fType = "", std::string exp = "", std::string sugg = "");

    // Overload the << operator declaration
    friend std::ostream& operator<<(std::ostream& os, const DetailedSyntaxError& error);
};


// --- Tokenizer Section ---

// Enum defining the different types of tokens the tokenizer can produce.
enum class TokenType {
    IDENTIFIER,       // e.g., variable names like 'x', 'count'
    NUMBER,           // e.g., '123', '0'
    KEYWORD,          // e.g., 'read', 'write', 'if', 'then', 'else', 'repeat', 'until', 'end'
    OPERATOR,         // e.g., ':=', '+', '-', '=', '>'
    PUNCTUATION,      // e.g., ';'
    END_OF_FILE,      // Special token indicating the end of the input code.
    INVALID           // Token representing a lexical error (e.g., unterminated comment, illegal character).
};

// Helper function to convert TokenType enum to a string representation.
std::string tokenTypeToString(TokenType type);

// Structure representing a single token identified by the tokenizer.
struct Token {
    TokenType type;       // The type of the token.
    std::string value;    // The actual string value of the token (e.g., "if", "x", "123", ":=").
    int line;             // The line number in the source code where the token begins.

    // Constructor declaration
    Token(TokenType t, std::string v, int l);

    // Overload the << operator declaration
    friend std::ostream& operator<<(std::ostream& os, const Token& token);
};

// Set of reserved keywords in the tiny language. Declared as extern.
extern const std::unordered_set<std::string> keywords;

// Function to check if a given string is a reserved keyword.
bool isKeyword(const std::string& s);

// Tokenizer function: Takes the source code as a string and returns a vector of Tokens.
std::vector<Token> tokenize(const std::string& code);


// --- Parser Section (Recursive Descent with Error Recovery) ---

// Structure for Abstract Syntax Tree (AST) nodes.
struct ASTNode {
    std::string nodeType;         // Type of the node (e.g., "Program", "IfStatement", "Identifier").
    std::string value;            // Value associated with the node (e.g., identifier name, number value, operator symbol).
    std::vector<ASTNode*> children; // Child nodes representing sub-structures.
    int line;                     // Line number where this node's construct begins in the source code.
    bool fromError = false;       // Flag indicating if this node or its children were parsed amidst errors.

    // Constructor declaration
    ASTNode(std::string type, int l, std::string val = "");

    // Virtual destructor declaration
    virtual ~ASTNode();

    // Prevent copying to avoid double deletion issues.
    ASTNode(const ASTNode&) = delete;
    ASTNode& operator=(const ASTNode&) = delete;
};

// Forward declarations of the mutually recursive parsing functions.
// These functions will now accept a pointer to a vector<string> for parsing steps.
ASTNode* parseProgram(const std::vector<Token>& tokens, std::vector<DetailedSyntaxError>& errors, std::vector<std::string>* parsingSteps);
ASTNode* parseStatementSequence(const std::vector<Token>& tokens, size_t& index, std::vector<DetailedSyntaxError>& errors, const std::set<std::string>& recoverySet, std::vector<std::string>* parsingSteps);
ASTNode* parseStatement(const std::vector<Token>& tokens, size_t& index, std::vector<DetailedSyntaxError>& errors, std::vector<std::string>* parsingSteps);
ASTNode* parseIfStatement(const std::vector<Token>& tokens, size_t& index, std::vector<DetailedSyntaxError>& errors, std::vector<std::string>* parsingSteps);
ASTNode* parseRepeatStatement(const std::vector<Token>& tokens, size_t& index, std::vector<DetailedSyntaxError>& errors, std::vector<std::string>* parsingSteps);
ASTNode* parseAssignStatement(const std::vector<Token>& tokens, size_t& index, std::vector<DetailedSyntaxError>& errors, std::vector<std::string>* parsingSteps);
ASTNode* parseReadStatement(const std::vector<Token>& tokens, size_t& index, std::vector<DetailedSyntaxError>& errors, std::vector<std::string>* parsingSteps);
ASTNode* parseWriteStatement(const std::vector<Token>& tokens, size_t& index, std::vector<DetailedSyntaxError>& errors, std::vector<std::string>* parsingSteps);
ASTNode* parseComparison(const std::vector<Token>& tokens, size_t& index, std::vector<DetailedSyntaxError>& errors, std::vector<std::string>* parsingSteps);
ASTNode* parseExpression(const std::vector<Token>& tokens, size_t& index, std::vector<DetailedSyntaxError>& errors, std::vector<std::string>* parsingSteps);
ASTNode* parseTerm(const std::vector<Token>& tokens, size_t& index, std::vector<DetailedSyntaxError>& errors, std::vector<std::string>* parsingSteps);
ASTNode* parseFactor(const std::vector<Token>& tokens, size_t& index, std::vector<DetailedSyntaxError>& errors, std::vector<std::string>* parsingSteps);


// Helper function to get the current token safely. Declaration only.
const Token& currentToken(const std::vector<Token>& tokens, size_t index);

// Synchronization Sets: Define sets of tokens for error recovery. Declared as extern.
extern const std::set<std::string> statementRecoverySet;
extern const std::set<std::string> expressionRecoverySet;
extern const std::set<std::string> comparisonRecoverySet;

// Error Recovery Function. Declaration only.
void synchronize(const std::vector<Token>& tokens, size_t& index, const std::set<std::string>& recoveryKeywords, std::vector<std::string>* parsingSteps);

// Match Function: Core parsing primitive. Declaration only.
bool match(const std::vector<Token>& tokens, size_t& index, TokenType expectedType, std::vector<DetailedSyntaxError>& errors, const std::string& expectedValue = "", const std::string& expectedTypeDesc = "", std::vector<std::string>* parsingSteps = nullptr);


// Structure to hold the results of parsing (AST root, errors, and steps).
struct ParseResult {
    ASTNode* astRoot = nullptr;
    std::vector<DetailedSyntaxError> errors;
    std::vector<std::string> steps; // Include steps member

    // Destructor to clean up the AST if it was created.
    ~ParseResult();

    // Prevent copying to avoid double deletion.
    ParseResult(const ParseResult&) = delete;
    ParseResult& operator=(const ParseResult&) = delete;

    // Allow moving.
    ParseResult(ParseResult&& other) noexcept;
    ParseResult& operator=(ParseResult&& other) noexcept;

    // Default constructor needed for creating an empty ParseResult.
    ParseResult() = default;
};


// Main parsing entry point function. Declaration only.
// Takes tokens and fills a ParseResult structure with the AST, errors, and steps.
void parseTokens(const std::vector<Token>& tokens, ParseResult& result);

// AST Printing Utility - Declaration only (if you still need it for native debugging)
// Note: This is not used by the C++/CLI GUI output directly.
void printAST(const ASTNode* node, int indent = 0);
