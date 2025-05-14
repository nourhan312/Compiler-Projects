#include "MyForm.h"
#include <sstream>
#include <msclr/marshal_cppstd.h>


using namespace TinyLangGUI;

// Helper function to format tokens as string

std::string formatTokens(const std::vector<Token>& tokens) {
    std::stringstream ss;
    ss << "TOKENS:\n";
    for (const auto& t : tokens) {
        ss << "Line " << t.line << ": " << tokenTypeToString(t.type) << " '" << t.value << "'\n";
    }
    return ss.str();
}

// Helper function to format parsing steps and errors as string

std::string formatParsing(const std::vector<Token>& tokens) {
    std::stringstream ss;

    
    size_t idx = 0;
    bool errorFound = false;
    ss << "\n--- Parsing Steps ---\n";
    while (idx < tokens.size()) {
        const Token& t = tokens[idx];
        if (t.type == TokenType::END_OF_FILE) break;
        if (t.type == TokenType::INVALID) {
            ss << "ERROR (Line " << t.line << "): Invalid token '" << t.value << "'\n";
            errorFound = true;
            idx++;
            continue;
        }
        // Accept KEYWORD statements ( read, write, if, repeat, ...)
        if (t.type == TokenType::KEYWORD) {
            ss << "Line " << t.line << ": Recognized KEYWORD '" << t.value << "'\n";
            idx++;
            // For demo: expect semicolon after every statement
            if (idx < tokens.size() && tokens[idx].type == TokenType::PUNCTUATION) {
                ss << "Line " << tokens[idx].line << ": Found ';'\n";
                idx++;
            }
            else {
                ss << "ERROR (Line " << t.line << "): Missing semicolon after '" << t.value << "'\n";
                errorFound = true;
            }
            continue;
        }
        // Accept assignment: IDENTIFIER := (NUMBER|IDENTIFIER) ;
        if (t.type == TokenType::IDENTIFIER) {
            ss << "Line " << t.line << ": Recognized IDENTIFIER '" << t.value << "'\n";
            idx++;
            if (idx < tokens.size() && tokens[idx].type == TokenType::OPERATOR && tokens[idx].value == ":=") {
                ss << "Line " << tokens[idx].line << ": Found ':='\n";
                idx++;
                if (idx < tokens.size() && (tokens[idx].type == TokenType::IDENTIFIER || tokens[idx].type == TokenType::NUMBER)) {
                    ss << "Line " << tokens[idx].line << ": Found right-hand side '" << tokens[idx].value << "'\n";
                    idx++;
                    if (idx < tokens.size() && tokens[idx].type == TokenType::PUNCTUATION) {
                        ss << "Line " << tokens[idx].line << ": Found ';'\n";
                        idx++;
                    }
                    else {
                        ss << "ERROR (Line " << t.line << "): Missing semicolon after assignment\n";
                        errorFound = true;
                    }
                }
                else {
                    ss << "ERROR (Line " << t.line << "): Missing right-hand side after ':='\n";
                    errorFound = true;
                }
            }
            else {
                ss << "ERROR (Line " << t.line << "): Expected ':=' after identifier\n";
                errorFound = true;
            }
            continue;
        }

        // Unexpected token

        ss << "ERROR (Line " << t.line << "): Unexpected token '" << t.value << "'\n";
        errorFound = true;
        idx++;
    }
    if (!errorFound) {
        ss << "\nNo syntax errors detected!\n";
    }
    return ss.str();
}

