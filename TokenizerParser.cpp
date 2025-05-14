#include "TokenizerParser.h"
#include <sstream>
using namespace std;

string errorTypeToString(ErrorType t) {
    switch (t) {
    case ErrorType::MISSING_SEMICOLON: return "Missing Semicolon";
    case ErrorType::MISSING_KEYWORD: return "Missing Keyword";
    case ErrorType::MISSING_IDENTIFIER: return "Missing Identifier";
    case ErrorType::MISSING_OPERATOR: return "Missing Operator";
    case ErrorType::MISSING_EXPRESSION: return "Missing Expression";
    case ErrorType::MISSING_TERM: return "Missing Term/Factor";
    case ErrorType::MISSING_COMPARISON: return "Missing Comparison";
    case ErrorType::UNEXPECTED_TOKEN: return "Unexpected Token";
    case ErrorType::INVALID_TOKEN: return "Invalid Token";
    case ErrorType::UNEXPECTED_END: return "Unexpected End of Input";
    case ErrorType::IDENTIFIER_NOT_ASSIGNMENT: return "Identifier Not Assignment";
    case ErrorType::INTERNAL_ERROR: return "Internal Parser Error";
    default: return "Unknown Error";
    }
}

DetailedSyntaxError::DetailedSyntaxError(string msg, int l, ErrorType t, string fVal, string fType, string exp, string sugg)
    : message(std::move(msg)), line(l), type(t), foundValue(std::move(fVal)), foundType(std::move(fType)), expectedDesc(std::move(exp)), suggestion(std::move(sugg)) {
}

ostream& operator<<(ostream& os, const DetailedSyntaxError& error) {
    os << "-\n";
    os << "  Type:      " << errorTypeToString(error.type) << " (Line " << error.line << ")\n"
        << "  Message:   " << error.message << "\n";
    if (!error.foundValue.empty() || !error.foundType.empty()) {
        os << "  Found:     '" + error.foundValue + "' (" + error.foundType + ")\n";
    }
    if (!error.expectedDesc.empty()) {
        os << "  Expected:  " << error.expectedDesc << "\n";
    }
    if (!error.suggestion.empty()) {
        os << "  Suggestion: " << error.suggestion << "\n";
    }
    return os;
}

string tokenTypeToString(TokenType type) {
    switch (type) {
    case TokenType::IDENTIFIER: return "IDENTIFIER";
    case TokenType::NUMBER:     return "NUMBER";
    case TokenType::KEYWORD:    return "KEYWORD";
    case TokenType::OPERATOR:   return "OPERATOR";
    case TokenType::PUNCTUATION:return "PUNCTUATION";
    case TokenType::END_OF_FILE:return "END_OF_FILE";
    case TokenType::INVALID:    return "INVALID";
    default:                    return "UNKNOWN";
    }
}

Token::Token(TokenType t, string v, int l) : type(t), value(std::move(v)), line(l) {}

ostream& operator<<(ostream& os, const Token& token) {
    os << "Token(" << tokenTypeToString(token.type) << ", '" << token.value << "', Line: " << token.line << ")";
    return os;
}

const unordered_set<string> keywords = {
    "read", "write", "if", "then", "else", "repeat", "until", "end"
};

bool isKeyword(const string& s) {
    return keywords.count(s);
}

vector<Token> tokenize(const string& code) {
    vector<Token> tokens;
    int line = 1;
    size_t pos = 0;
    while (pos < code.size()) {
        char current_char = code[pos];
        if (isspace(current_char)) {
            if (current_char == '\n') line++;
            pos++;
            continue;
        }
        if (current_char == '{') {
            size_t commentStartPos = pos;
            int commentStartLine = line;
            pos++;
            while (pos < code.size() && code[pos] != '}') {
                if (code[pos] == '\n') line++;
                pos++;
            }
            if (pos < code.size()) {
                pos++;
            }
            else {
                tokens.push_back(Token(TokenType::INVALID, "{ Unterminated comment starting", commentStartLine));
            }
            continue;
        }
        if (isalpha(current_char)) {
            string identifier;
            identifier += current_char;
            pos++;
            while (pos < code.size() && isalnum(code[pos])) {
                identifier += code[pos];
                pos++;
            }
            tokens.push_back(Token(isKeyword(identifier) ? TokenType::KEYWORD : TokenType::IDENTIFIER, identifier, line));
            continue;
        }
        if (isdigit(current_char)) {
            string number;
            number += current_char;
            pos++;
            while (pos < code.size() && isdigit(code[pos])) {
                number += code[pos];
                pos++;
            }
            tokens.push_back(Token(TokenType::NUMBER, number, line));
            continue;
        }
        switch (current_char) {
        case ':':
            if (pos + 1 < code.size() && code[pos + 1] == '=') {
                tokens.push_back(Token(TokenType::OPERATOR, ":=", line));
                pos += 2;
            }
            else {
                tokens.push_back(Token(TokenType::INVALID, ":", line));
                pos++;
            }
            break;
        case '+':
        case '-':
        case '=':
        case '<':
        case '*':
            tokens.push_back(Token(TokenType::OPERATOR, string(1, current_char), line));
            pos++;
            break;
        case ';':
            tokens.push_back(Token(TokenType::PUNCTUATION, ";", line));
            pos++;
            break;
        default:
            tokens.push_back(Token(TokenType::INVALID, string(1, current_char), line));
            pos++;
            break;
        }
    }
    tokens.push_back(Token(TokenType::END_OF_FILE, "", line));
    return tokens;
}

ASTNode::ASTNode(string type, int l, string val) : nodeType(std::move(type)), line(l), value(std::move(val)) {}

ASTNode::~ASTNode() {
    for (ASTNode* child : children) {
        delete child;
    }
}

const Token& currentToken(const vector<Token>& tokens, size_t index) {
    if (index >= tokens.size() - 1) {
        return tokens.back();
    }
    return tokens[index];
}

const set<string> statementRecoverySet = { "read", "write", "if", "repeat", "else", "until", "end" };
const set<string> expressionRecoverySet = { ";", "then", "else", "until", "end", ")", "=", "<" };
const set<string> comparisonRecoverySet = { ";", "end", "then" };

void synchronize(const vector<Token>& tokens, size_t& index, const set<string>& recoveryKeywords, vector<string>* parsingSteps) {
    size_t startIndex = index;
    if (parsingSteps) {
        std::stringstream ss_sync;
        ss_sync << "[Sync] Attempting sync from line " << currentToken(tokens, index).line << " (token: '" << currentToken(tokens, index).value << "'). Recovery set: {";
        bool first = true;
        for (const auto& k : recoveryKeywords) {
            if (!first) ss_sync << ", ";
            ss_sync << k;
            first = false;
        }
        ss_sync << "}";
        (*parsingSteps).push_back(ss_sync.str());
    }
    while (currentToken(tokens, index).type != TokenType::END_OF_FILE) {
        const Token& current = currentToken(tokens, index);
        if (recoveryKeywords.count(current.value)) {
            if (parsingSteps) (*parsingSteps).push_back("[Sync] Synchronized before token '" + current.value + "' at line " + to_string(current.line));
            return;
        }
        if (index + 1 < tokens.size() - 1) {
            const Token& next = currentToken(tokens, index + 1);
            if (recoveryKeywords.count(next.value)) {
                if (parsingSteps) (*parsingSteps).push_back("[Sync] Synchronized by lookahead before next token '" + next.value + "' at line " + to_string(next.line) + " (skipping '" + current.value + "')");
                index++;
                return;
            }
        }
        if (parsingSteps) (*parsingSteps).push_back("[Sync] Skipping token '" + current.value + "' at line " + to_string(current.line));
        index++;
        if (index == startIndex && index < tokens.size() - 1) {
            if (parsingSteps) (*parsingSteps).push_back("[Sync] Warning: Synchronization stalled, forcing advance.");
            index++;
        }
        startIndex = index;
    }
    if (parsingSteps) (*parsingSteps).push_back("[Sync] Synchronization reached END_OF_FILE.");
}

bool match(const vector<Token>& tokens, size_t& index, TokenType expectedType, vector<DetailedSyntaxError>& errors, const string& expectedValue, const string& expectedTypeDesc, vector<string>* parsingSteps) {
    const Token& token = currentToken(tokens, index);
    if (token.type == TokenType::INVALID) {
        if (parsingSteps) {
            (*parsingSteps).push_back("Match failed: Found INVALID token '" + token.value + "' at line " + to_string(token.line));
        }
        if (errors.empty() || errors.back().line != token.line || errors.back().foundValue != token.value || errors.back().type != ErrorType::INVALID_TOKEN) {
            errors.emplace_back(
                "Invalid token '" + token.value + "' encountered during parsing.",
                token.line, ErrorType::INVALID_TOKEN, token.value, "INVALID",
                (expectedValue.empty() ? (expectedTypeDesc.empty() ? tokenTypeToString(expectedType) : expectedTypeDesc) : "'" + expectedValue + "'"),
                "Remove or replace the invalid token '" + token.value + "'."
            );
        }
        index++;
        return false;
    }
    if (token.type != expectedType || (!expectedValue.empty() && token.value != expectedValue)) {
        if (parsingSteps) {
            (*parsingSteps).push_back("Match failed: Expected " + (expectedValue.empty() ? expectedTypeDesc : "'" + expectedValue + "'") + " but found '" + token.value + "' at line " + to_string(token.line));
        }
        string expectedStr;
        string suggestionStr;
        ErrorType errType = ErrorType::UNEXPECTED_TOKEN;
        if (!expectedValue.empty()) {
            expectedStr = "'" + expectedValue + "'";
            if (expectedType == TokenType::KEYWORD) expectedStr = "keyword " + expectedStr;
            else if (expectedType == TokenType::OPERATOR) expectedStr = "operator " + expectedStr;
            else if (expectedType == TokenType::PUNCTUATION) expectedStr = "punctuation " + expectedStr;
            if (expectedValue == ";") {
                errType = ErrorType::MISSING_SEMICOLON;
                expectedStr = "semicolon ';'";
                if (statementRecoverySet.count(token.value) || token.type == TokenType::IDENTIFIER) {
                    suggestionStr = "Insert a semicolon ';' before '" + token.value + "'.";
                }
                else {
                    suggestionStr = "Replace '" + token.value + "' with ';'.";
                }
            }
            else if (expectedValue == ":=") {
                errType = ErrorType::MISSING_OPERATOR;
                expectedStr = "assignment operator ':='";
                suggestionStr = "Replace '" + token.value + "' with ':='.";
            }
            else if (expectedValue == "=" || expectedValue == "<") {
                errType = ErrorType::MISSING_OPERATOR;
                expectedStr = "comparison operator ('=' or '<')";
                suggestionStr = "Replace '" + token.value + "' with '=' or '<'.";
            }
            else if (expectedType == TokenType::KEYWORD) {
                errType = ErrorType::MISSING_KEYWORD;
                suggestionStr = "Replace '" + token.value + "' with the keyword '" + expectedValue + "'.";
            }
            else {
                suggestionStr = "Replace '" + token.value + "' with " + expectedStr + ".";
            }
        }
        else if (!expectedTypeDesc.empty()) {
            expectedStr = expectedTypeDesc;
            suggestionStr = "Expected " + expectedStr + " but found '" + token.value + "'. Check syntax.";
            if (expectedType == TokenType::IDENTIFIER) errType = ErrorType::MISSING_IDENTIFIER;
            else if (expectedType == TokenType::NUMBER) errType = ErrorType::MISSING_TERM;
            else if (expectedType == TokenType::OPERATOR) errType = ErrorType::MISSING_OPERATOR;
        }
        else {
            expectedStr = tokenTypeToString(expectedType);
            suggestionStr = "Expected token of type " + expectedStr + " but found '" + token.value + "'.";
        }
        if (errors.empty() || errors.back().line != token.line || errors.back().foundValue != token.value || errors.back().expectedDesc != expectedStr) {
            errors.emplace_back(
                "Expected " + expectedStr + " but got '" + token.value + "'.",
                token.line, errType, token.value, tokenTypeToString(token.type),
                expectedStr, suggestionStr
            );
        }
        return false;
    }

    // Success

    if (parsingSteps) {
        (*parsingSteps).push_back("Matched " + tokenTypeToString(token.type) + " '" + token.value + "' at line " + to_string(token.line));
    }
    index++;
    return true;
}




ASTNode* parseProgram(const vector<Token>& tokens, vector<DetailedSyntaxError>& errors, vector<string>* parsingSteps) {
    if (parsingSteps) (*parsingSteps).push_back("-> Entering parseProgram");
    size_t currentTokenIndex = 0;
    ASTNode* programNode = new ASTNode("Program", 0);
    bool hasErrors = false;

    if (tokens.empty() || (tokens.size() == 1 && tokens[0].type == TokenType::END_OF_FILE)) {
        if (parsingSteps) (*parsingSteps).push_back("Program is empty or only contains EOF.");
        if (parsingSteps) (*parsingSteps).push_back("<- Exiting parseProgram");
        return programNode;
    }
    set<string> topLevelRecovery = statementRecoverySet;
    ASTNode* sequence = parseStatementSequence(tokens, currentTokenIndex, errors, topLevelRecovery, parsingSteps);

    if (sequence) {
        programNode->children.push_back(sequence);
        if (sequence->fromError) hasErrors = true;
    }
    else {
        if (parsingSteps) (*parsingSteps).push_back("Failed to parse initial StatementSequence.");
        if (errors.empty() || errors.back().type != ErrorType::UNEXPECTED_END) {
            if (currentToken(tokens, currentTokenIndex).type != TokenType::END_OF_FILE) {
                const Token& current = currentToken(tokens, currentTokenIndex);
                errors.emplace_back(
                    "Program does not start with a valid statement.",
                    current.line, ErrorType::UNEXPECTED_TOKEN,
                    current.value, tokenTypeToString(current.type),
                    "start of statement (read, write, if, repeat, identifier)",
                    "Begin the program with a valid statement."
                );
            }
        }
        hasErrors = true;
    }
    if (currentToken(tokens, currentTokenIndex).type != TokenType::END_OF_FILE) {
        if (parsingSteps) (*parsingSteps).push_back("Expected END_OF_FILE but found more tokens.");
        const Token& extraToken = currentToken(tokens, currentTokenIndex);
        if (errors.empty() || errors.back().line != extraToken.line ||
            (errors.back().type != ErrorType::UNEXPECTED_END && errors.back().type != ErrorType::INVALID_TOKEN))
        {
            errors.emplace_back(
                "Expected end of program but found more tokens starting with '" + extraToken.value + "'.",
                extraToken.line, ErrorType::UNEXPECTED_END, extraToken.value, tokenTypeToString(extraToken.type),
                "end of program",
                "Remove unexpected tokens after the main program code, or ensure blocks like 'if'/'repeat' are properly closed with 'end'/'until'."
            );
            hasErrors = true;
            synchronize(tokens, currentTokenIndex, {}, parsingSteps);
        }
    }
    else {
        if (parsingSteps) (*parsingSteps).push_back("Matched END_OF_FILE.");
    }
    programNode->fromError = hasErrors || !errors.empty();
    programNode->children.erase(remove(programNode->children.begin(), programNode->children.end(), nullptr), programNode->children.end());
    if (parsingSteps) (*parsingSteps).push_back("<- Exiting parseProgram");
    return programNode;
}

ASTNode* parseStatementSequence(const vector<Token>& tokens, size_t& index, vector<DetailedSyntaxError>& errors, const set<string>& recoverySet, vector<string>* parsingSteps) {
    if (parsingSteps) (*parsingSteps).push_back("-> Entering parseStatementSequence at line " + to_string(currentToken(tokens, index).line));
    int firstLine = currentToken(tokens, index).line;
    ASTNode* seqNode = new ASTNode("StatementSequence", firstLine);
    bool sequenceHasErrors = false;

    while (true) {
        const Token& current = currentToken(tokens, index);

        // Exit conditions: EOF or block terminators

        if (current.type == TokenType::END_OF_FILE ||
            (current.type == TokenType::KEYWORD &&
                (current.value == "until" || current.value == "end" || current.value == "else"))) {
            if (parsingSteps) (*parsingSteps).push_back("StatementSequence terminated by '" + current.value + "' at line " + to_string(current.line));
            break;
        }

        size_t indexBeforeStatement = index;
        ASTNode* stmt = parseStatement(tokens, index, errors, parsingSteps);
        if (stmt) {
            seqNode->children.push_back(stmt);
            if (stmt->fromError) sequenceHasErrors = true;
            const Token& next = currentToken(tokens, index);

            // Check for semicolon after the statement
            if (next.type == TokenType::PUNCTUATION && next.value == ";") {
                if (currentToken(tokens, index + 1).type == TokenType::END_OF_FILE) {
                    // Ignore trailing semicolon at the end of the program
                    if (parsingSteps) (*parsingSteps).push_back("Ignoring trailing semicolon at EOF.");
                    index++; // Skip the trailing semicolon
                }
                else {
                    // Normal semicolon between statements
                    if (parsingSteps) (*parsingSteps).push_back("Statement followed by ';'.");
                    match(tokens, index, TokenType::PUNCTUATION, errors, ";", "semicolon ';'", parsingSteps);
                }
            }
            else {
                // Report missing semicolon error

                if (parsingSteps) (*parsingSteps).push_back("Missing ';' after statement at line " + to_string(stmt->line) + ". Found '" + next.value + "'");
                errors.emplace_back(
                    "Missing semicolon ';' after statement.",
                    next.line, ErrorType::MISSING_SEMICOLON,
                    next.value, tokenTypeToString(next.type),
                    "';'",
                    "Add a semicolon after the statement."
                );
                sequenceHasErrors = true;
                if (parsingSteps) (*parsingSteps).push_back("Attempting synchronization after missing ';'.");
                synchronize(tokens, index, recoverySet, parsingSteps);
            }
        }
        else {
            // Failed to parse a statement
            sequenceHasErrors = true;
            if (parsingSteps) (*parsingSteps).push_back("Failed to parse a Statement at line " + to_string(currentToken(tokens, index).line) + ".");
            if (index == indexBeforeStatement && currentToken(tokens, index).type != TokenType::END_OF_FILE) {
                if (parsingSteps) (*parsingSteps).push_back("Attempting synchronization from line " + to_string(currentToken(tokens, index).line) + ".");
                synchronize(tokens, index, recoverySet, parsingSteps);
                if (parsingSteps) (*parsingSteps).push_back("Synchronization reached line " + to_string(currentToken(tokens, index).line) + ".");
                if (index == indexBeforeStatement && index < tokens.size() - 1) {
                    if (parsingSteps) (*parsingSteps).push_back("Synchronization stalled, forcing advance.");
                    index++;
                }
            }
        }
    }

    seqNode->fromError = sequenceHasErrors;
    seqNode->children.erase(remove(seqNode->children.begin(), seqNode->children.end(), nullptr), seqNode->children.end());
    if (seqNode->children.empty() && sequenceHasErrors) {
        if (parsingSteps) (*parsingSteps).push_back("StatementSequence is empty and had errors, returning nullptr.");
        delete seqNode;
        return nullptr;
    }
    if (parsingSteps) (*parsingSteps).push_back("<- Exiting parseStatementSequence");
    return seqNode;
}

ASTNode* parseStatement(const vector<Token>& tokens, size_t& index, vector<DetailedSyntaxError>& errors, vector<string>* parsingSteps) {

    const Token& token = currentToken(tokens, index);

    if (parsingSteps) (*parsingSteps).push_back("-> Entering parseStatement starting with '" + token.value + "' at line " + to_string(token.line));

    ASTNode* statementNode = nullptr;
    bool statementHasError = false;


    if (token.type == TokenType::INVALID) {
        if (parsingSteps) (*parsingSteps).push_back("Statement starts with INVALID token.");
        if (errors.empty() || errors.back().line != token.line || errors.back().foundValue != token.value || errors.back().type != ErrorType::INVALID_TOKEN) {
            errors.emplace_back(
                "Invalid token '" + token.value + "' cannot start a statement.",
                token.line, ErrorType::INVALID_TOKEN, token.value, "INVALID",
                "start of statement (read, write, if, repeat, identifier)",
                "Remove or replace the invalid token '" + token.value + "'."
            );
        }
        index++;
        if (parsingSteps) (*parsingSteps).push_back("<- Exiting parseStatement (Invalid Token)");
        return nullptr;
    }
    try {
        if (token.type == TokenType::KEYWORD) {
            if (token.value == "if") statementNode = parseIfStatement(tokens, index, errors, parsingSteps);
            else if (token.value == "repeat") statementNode = parseRepeatStatement(tokens, index, errors, parsingSteps);
            else if (token.value == "read") statementNode = parseReadStatement(tokens, index, errors, parsingSteps);
            else if (token.value == "write") statementNode = parseWriteStatement(tokens, index, errors, parsingSteps);
            else {
                if (parsingSteps) (*parsingSteps).push_back("Unexpected keyword '" + token.value + "' starting statement.");
                if (errors.empty() || errors.back().line != token.line || errors.back().foundValue != token.value || errors.back().type != ErrorType::UNEXPECTED_TOKEN) {
                    errors.emplace_back(
                        "Unexpected keyword '" + token.value + "' found where a statement should begin.",
                        token.line, ErrorType::UNEXPECTED_TOKEN, token.value, "KEYWORD",
                        "start of statement (if, repeat, read, write, identifier)",
                        "Ensure keywords like 'then', 'else', 'until', 'end' are used only in their correct structural positions."
                    );
                }
                statementHasError = true;
                if (parsingSteps) (*parsingSteps).push_back("<- Exiting parseStatement (Unexpected Keyword)");
                return nullptr;
            }
        }
        else if (token.type == TokenType::IDENTIFIER) {
            if (parsingSteps) (*parsingSteps).push_back("Statement starts with IDENTIFIER, checking for assignment.");
            if (index + 1 < tokens.size() - 1 && currentToken(tokens, index + 1).type == TokenType::OPERATOR && currentToken(tokens, index + 1).value == ":=") {
                statementNode = parseAssignStatement(tokens, index, errors, parsingSteps);
            }
            else {
                if (parsingSteps) (*parsingSteps).push_back("IDENTIFIER not followed by ':='.");
                if (errors.empty() || errors.back().line != token.line || errors.back().foundValue != token.value || errors.back().type != ErrorType::IDENTIFIER_NOT_ASSIGNMENT) {
                    errors.emplace_back(
                        "Identifier '" + token.value + "' is not followed by assignment operator ':='.",
                        token.line, ErrorType::IDENTIFIER_NOT_ASSIGNMENT, token.value, "IDENTIFIER",
                        "assignment operator ':='",
                        "If this should be an assignment, add ':= expression;'. Otherwise, check for a misplaced identifier or misspelled keyword."
                    );
                }
                statementHasError = true;
                if (parsingSteps) (*parsingSteps).push_back("<- Exiting parseStatement (Identifier Not Assignment)");
                return nullptr;
            }
        }
        else if (token.type == TokenType::END_OF_FILE) {
            if (parsingSteps) (*parsingSteps).push_back("Unexpected END_OF_FILE while parsing Statement.");
            if (errors.empty() || errors.back().type != ErrorType::UNEXPECTED_END) {
                errors.emplace_back(
                    "Unexpected end of input found where a statement was expected.",
                    token.line, ErrorType::UNEXPECTED_END, "", "END_OF_FILE",
                    "statement (if, repeat, read, write, assignment)",
                    "Check for incomplete code, missing statements, or unclosed blocks ('end', 'until')."
                );
            }
            statementHasError = true;
            if (parsingSteps) (*parsingSteps).push_back("<- Exiting parseStatement (Unexpected EOF)");
            return nullptr;
        }
        else {
            if (parsingSteps) (*parsingSteps).push_back("Unexpected token type starting Statement.");
            if (errors.empty() || errors.back().line != token.line || errors.back().foundValue != token.value || errors.back().type != ErrorType::UNEXPECTED_TOKEN) {
                errors.emplace_back(
                    "Unexpected token '" + token.value + "' (" + tokenTypeToString(token.type) + ") cannot start a statement.",
                    token.line, ErrorType::UNEXPECTED_TOKEN, token.value, tokenTypeToString(token.type),
                    "start of statement (keyword or identifier)",
                    "Remove misplaced token '" + token.value + "' or start with a valid statement (if, repeat, read, write, identifier := ...)."
                );
            }
            statementHasError = true;
            if (parsingSteps) (*parsingSteps).push_back("<- Exiting parseStatement (Unexpected Token Type)");
            return nullptr;
        }
    }
    catch (const std::exception& e) {
        if (parsingSteps) (*parsingSteps).push_back("Caught C++ exception during Statement parsing: " + string(e.what()));
        errors.emplace_back("Internal Parser Error: " + string(e.what()), currentToken(tokens, index).line, ErrorType::INTERNAL_ERROR);
        statementHasError = true;
        if (parsingSteps) (*parsingSteps).push_back("<- Exiting parseStatement (C++ Exception)");
        return nullptr;
    }
    catch (...) {
        if (parsingSteps) (*parsingSteps).push_back("Caught unknown exception during Statement parsing.");
        errors.emplace_back("Unknown Internal Parser Error occurred.", currentToken(tokens, index).line, ErrorType::INTERNAL_ERROR);
        statementHasError = true;
        if (parsingSteps) (*parsingSteps).push_back("<- Exiting parseStatement (Unknown Exception)");
        return nullptr;
    }
    if (!statementNode) {
        statementHasError = true;
    }
    else if (statementNode->fromError) {
        statementHasError = true;
    }
    if (statementNode) {
        statementNode->fromError = statementHasError;
    }
    if (parsingSteps) (*parsingSteps).push_back("<- Exiting parseStatement");
    return statementNode;
}

ASTNode* parseReadStatement(const vector<Token>& tokens, size_t& index, vector<DetailedSyntaxError>& errors, vector<string>* parsingSteps) {
    int line = currentToken(tokens, index).line;
    if (parsingSteps) (*parsingSteps).push_back("-> Entering parseReadStatement at line " + to_string(line));
    bool nodeHasError = false;

    if (!match(tokens, index, TokenType::KEYWORD, errors, "read", "keyword 'read'", parsingSteps)) {
        if (parsingSteps) (*parsingSteps).push_back("Failed to match 'read'.");
        if (parsingSteps) (*parsingSteps).push_back("<- Exiting parseReadStatement (Match Failed)");
        return nullptr;
    }

    if (parsingSteps) (*parsingSteps).push_back("Matched 'read'.");
    const Token& idTokenBeforeMatch = currentToken(tokens, index);
    if (!match(tokens, index, TokenType::IDENTIFIER, errors, "", "identifier (variable name)", parsingSteps)) {
        if (parsingSteps) (*parsingSteps).push_back("Failed to match identifier after 'read'.");
        nodeHasError = true;
        if (parsingSteps) (*parsingSteps).push_back("Attempting synchronization after missing identifier.");
        synchronize(tokens, index, statementRecoverySet, parsingSteps);
        if (parsingSteps) (*parsingSteps).push_back("Synchronization reached line " + to_string(currentToken(tokens, index).line) + ".");
        if (parsingSteps) (*parsingSteps).push_back("<- Exiting parseReadStatement (Missing Identifier)");
        return nullptr;
    }
    if (parsingSteps) (*parsingSteps).push_back("Matched identifier '" + idTokenBeforeMatch.value + "' after 'read'.");
    ASTNode* readNode = new ASTNode("ReadStatement", line);
    readNode->children.push_back(new ASTNode("Identifier", idTokenBeforeMatch.line, idTokenBeforeMatch.value));
    readNode->fromError = nodeHasError;
    if (parsingSteps) (*parsingSteps).push_back("<- Exiting parseReadStatement");
    return readNode;
}

ASTNode* parseWriteStatement(const vector<Token>& tokens, size_t& index, vector<DetailedSyntaxError>& errors, vector<string>* parsingSteps) {

    int line = currentToken(tokens, index).line;

    if (parsingSteps) (*parsingSteps).push_back("-> Entering parseWriteStatement at line " + to_string(line));

    bool nodeHasError = false;

    if (!match(tokens, index, TokenType::KEYWORD, errors, "write", "keyword 'write'", parsingSteps)) {
        if (parsingSteps) (*parsingSteps).push_back("Failed to match 'write'.");
        if (parsingSteps) (*parsingSteps).push_back("<- Exiting parseWriteStatement (Match Failed)");
        return nullptr;
    }

    if (parsingSteps) (*parsingSteps).push_back("Matched 'write'. Parsing Expression.");
    int exprExpectedLine = currentToken(tokens, index).line;
    ASTNode* expr = parseExpression(tokens, index, errors, parsingSteps);
    if (!expr) {
        if (parsingSteps) (*parsingSteps).push_back("Failed to parse Expression after 'write'.");
        if (errors.empty() || errors.back().line < exprExpectedLine ||
            (errors.back().line == exprExpectedLine &&
                errors.back().type != ErrorType::MISSING_TERM &&
                errors.back().type != ErrorType::MISSING_EXPRESSION &&
                errors.back().type != ErrorType::INVALID_TOKEN)) {
            const Token& current = currentToken(tokens, index);
            errors.emplace_back(
                "Missing expression after 'write' keyword.",
                exprExpectedLine, ErrorType::MISSING_EXPRESSION, current.value, tokenTypeToString(current.type),
                "expression (variable or number)",
                "Provide a variable name or a number to be written after 'write'."
            );
        }
        nodeHasError = true;
    }
    else {
        if (parsingSteps) (*parsingSteps).push_back("Parsed Expression after 'write'.");
        if (expr->fromError) nodeHasError = true;
    }
    ASTNode* writeNode = new ASTNode("WriteStatement", line);
    if (expr) {
        writeNode->children.push_back(expr);
    }
    writeNode->fromError = nodeHasError;
    if (parsingSteps) (*parsingSteps).push_back("<- Exiting parseWriteStatement");
    return writeNode;
}

ASTNode* parseAssignStatement(const vector<Token>& tokens, size_t& index, vector<DetailedSyntaxError>& errors, vector<string>* parsingSteps) {
    int line = currentToken(tokens, index).line;

    if (parsingSteps) (*parsingSteps).push_back("-> Entering parseAssignStatement at line " + to_string(line));
    bool nodeHasError = false;

    const Token& idToken = currentToken(tokens, index);
    if (!match(tokens, index, TokenType::IDENTIFIER, errors, "", "identifier (variable name)", parsingSteps)) {
        if (parsingSteps) (*parsingSteps).push_back("Failed to match identifier for assignment.");
        if (parsingSteps) (*parsingSteps).push_back("<- Exiting parseAssignStatement (Match Failed)");
        return nullptr;
    }
    if (parsingSteps) (*parsingSteps).push_back("Matched identifier '" + idToken.value + "' for assignment.");
    ASTNode* assignNode = new ASTNode("AssignStatement", line);
    assignNode->children.push_back(new ASTNode("Identifier", idToken.line, idToken.value));
    int assignOpLine = currentToken(tokens, index).line;
    bool matchedAssignmentOperator = match(tokens, index, TokenType::OPERATOR, errors, ":=", "assignment operator ':=', parsingSteps");
    if (!matchedAssignmentOperator) {
        if (parsingSteps) (*parsingSteps).push_back("Failed to match ':=' operator. Marking node as erroneous.");
        nodeHasError = true;
    }
    else {
        if (parsingSteps) (*parsingSteps).push_back("Matched ':=' operator.");
    }
    int exprExpectedLine = currentToken(tokens, index).line;
    ASTNode* expr = parseExpression(tokens, index, errors, parsingSteps);
    if (!expr) {
        if (parsingSteps) (*parsingSteps).push_back("Failed to parse Expression after ':='.");
        if (errors.empty() || errors.back().line < exprExpectedLine ||
            (errors.back().line == exprExpectedLine &&
                errors.back().type != ErrorType::MISSING_TERM &&
                errors.back().type != ErrorType::MISSING_EXPRESSION &&
                errors.back().type != ErrorType::INVALID_TOKEN)) {
            const Token& current = currentToken(tokens, index);
            errors.emplace_back(
                "Missing expression after assignment operator ':='.",
                exprExpectedLine, ErrorType::MISSING_EXPRESSION, current.value, tokenTypeToString(current.type),
                "expression (variable or number)",
                "Provide a variable or number for the assignment after ':='."
            );
        }
        nodeHasError = true;
    }
    else {
        if (parsingSteps) (*parsingSteps).push_back("Parsed Expression after ':='.");
        if (expr->fromError) nodeHasError = true;
        assignNode->children.push_back(expr);
    }
    assignNode->fromError = nodeHasError;
    if (parsingSteps) (*parsingSteps).push_back("<- Exiting parseAssignStatement");
    return assignNode;
}

ASTNode* parseExpression(const vector<Token>& tokens, size_t& index, vector<DetailedSyntaxError>& errors, vector<string>* parsingSteps) {
    int line = currentToken(tokens, index).line;
    if (parsingSteps) (*parsingSteps).push_back("-> Entering parseExpression at line " + to_string(line));
    bool nodeHasError = false;
    ASTNode* leftTerm = parseTerm(tokens, index, errors, parsingSteps);
    if (!leftTerm) {
        if (parsingSteps) (*parsingSteps).push_back("Failed to parse left Term in Expression.");
        if (errors.empty() || errors.back().line < line ||
            (errors.back().line == line &&
                errors.back().type != ErrorType::MISSING_TERM &&
                errors.back().type != ErrorType::INVALID_TOKEN &&
                errors.back().type != ErrorType::MISSING_EXPRESSION)) {
            const Token& current = currentToken(tokens, index);
            errors.emplace_back(
                "Invalid start of expression or sub-expression.",
                line, ErrorType::MISSING_EXPRESSION, current.value, tokenTypeToString(current.type),
                "expression starting with identifier or number",
                "Ensure the expression or sub-expression starts with a variable or a number."
            );
        }
        if (parsingSteps) (*parsingSteps).push_back("<- Exiting parseExpression (Failed Left Term)");
        return nullptr;
    }
    if (parsingSteps) (*parsingSteps).push_back("Parsed left Term in Expression.");
    if (leftTerm->fromError) nodeHasError = true;

    while (currentToken(tokens, index).type == TokenType::OPERATOR &&
        (currentToken(tokens, index).value == "+" || currentToken(tokens, index).value == "-" || currentToken(tokens, index).value == "*")) {
        const Token& opToken = currentToken(tokens, index);
        if (parsingSteps) (*parsingSteps).push_back("Found operator '" + opToken.value + "'. Parsing right Term.");
        if (!match(tokens, index, TokenType::OPERATOR, errors, opToken.value, "operator ('+', '-', or '*')", parsingSteps)) {
            if (parsingSteps) (*parsingSteps).push_back("Failed to match operator.");
            nodeHasError = true;
            if (leftTerm) leftTerm->fromError = true;
            if (parsingSteps) (*parsingSteps).push_back("<- Exiting parseExpression (Operator Match Failed)");
            return leftTerm;
        }

        int termExpectedLine = currentToken(tokens, index).line;
        ASTNode* rightTerm = parseTerm(tokens, index, errors, parsingSteps);
        if (!rightTerm) {
            if (parsingSteps) (*parsingSteps).push_back("Failed to parse right Term after operator.");
            if (errors.empty() || errors.back().line < termExpectedLine ||
                (errors.back().line == termExpectedLine &&
                    errors.back().type != ErrorType::MISSING_TERM &&
                    errors.back().type != ErrorType::MISSING_EXPRESSION &&
                    errors.back().type != ErrorType::INVALID_TOKEN)) {
                const Token& current = currentToken(tokens, index);
                errors.emplace_back(
                    "Missing term (identifier or number) after operator '" + opToken.value + "'.",
                    opToken.line, ErrorType::MISSING_TERM, current.value, tokenTypeToString(current.type),
                    "identifier or number",
                    "Provide a variable or number after the '" + opToken.value + "' operator."
                );
            }
            nodeHasError = true;
            ASTNode* exprNode = new ASTNode("BinaryOperation", opToken.line);
            exprNode->children.push_back(leftTerm);
            exprNode->children.push_back(new ASTNode("Operator", opToken.line, opToken.value));
            exprNode->fromError = true;
            if (parsingSteps) (*parsingSteps).push_back("<- Exiting parseExpression (Missing Right Term)");
            return exprNode;
        }
        if (parsingSteps) (*parsingSteps).push_back("Parsed right Term after operator.");
        if (rightTerm->fromError) nodeHasError = true;

        ASTNode* exprNode = new ASTNode("BinaryOperation", opToken.line);
        exprNode->children.push_back(leftTerm);
        exprNode->children.push_back(new ASTNode("Operator", opToken.line, opToken.value));
        exprNode->children.push_back(rightTerm);
        exprNode->fromError = nodeHasError;
        if (parsingSteps) (*parsingSteps).push_back("Created BinaryOperation node: " + exprNode->nodeType + " (" + exprNode->children[1]->value + ")");
        leftTerm = exprNode;
    }

    if (leftTerm) leftTerm->fromError = nodeHasError;
    if (parsingSteps) (*parsingSteps).push_back("<- Exiting parseExpression");
    return leftTerm;
}

ASTNode* parseTerm(const vector<Token>& tokens, size_t& index, vector<DetailedSyntaxError>& errors, vector<string>* parsingSteps) {
    if (parsingSteps) (*parsingSteps).push_back("-> Entering parseTerm at line " + to_string(currentToken(tokens, index).line));
    ASTNode* factor = parseFactor(tokens, index, errors, parsingSteps);
    if (parsingSteps && factor) (*parsingSteps).push_back("Parsed Factor for Term: " + factor->nodeType + (factor->value.empty() ? "" : " (" + factor->value + ")"));
    if (parsingSteps && !factor) (*parsingSteps).push_back("Failed to parse Factor for Term.");
    if (parsingSteps) (*parsingSteps).push_back("<- Exiting parseTerm");
    return factor;
}

ASTNode* parseFactor(const vector<Token>& tokens, size_t& index, vector<DetailedSyntaxError>& errors, vector<string>* parsingSteps) {
    const Token& token = currentToken(tokens, index);
    if (parsingSteps) (*parsingSteps).push_back("-> Entering parseFactor starting with '" + token.value + "' at line " + to_string(token.line));
    ASTNode* factorNode = nullptr;
    bool nodeHasError = false;
    if (token.type == TokenType::IDENTIFIER) {
        string value = token.value; int line = token.line;

        if (!match(tokens, index, TokenType::IDENTIFIER, errors, "", "identifier (variable name)", parsingSteps)) {
            if (parsingSteps) (*parsingSteps).push_back("Failed to match IDENTIFIER for Factor.");
            if (parsingSteps) (*parsingSteps).push_back("<- Exiting parseFactor (Match Failed)");
            return nullptr;
        }
        if (parsingSteps) (*parsingSteps).push_back("Matched IDENTIFIER '" + value + "' for Factor.");
        factorNode = new ASTNode("Identifier", line, value);
    }
    else if (token.type == TokenType::NUMBER) {
        string value = token.value; int line = token.line;
        if (!match(tokens, index, TokenType::NUMBER, errors, "", "number", parsingSteps)) {
            if (parsingSteps) (*parsingSteps).push_back("Failed to match NUMBER for Factor.");
            if (parsingSteps) (*parsingSteps).push_back("<- Exiting parseFactor (Match Failed)");
            return nullptr;
        }
        if (parsingSteps) (*parsingSteps).push_back("Matched NUMBER '" + value + "' for Factor.");
        factorNode = new ASTNode("Number", line, value);
    }
    else {
        if (parsingSteps) (*parsingSteps).push_back("Unexpected token '" + token.value + "' for Factor.");
        if (errors.empty() || errors.back().line < token.line ||
            (errors.back().line == token.line &&
                errors.back().type != ErrorType::MISSING_TERM &&
                errors.back().type != ErrorType::INVALID_TOKEN &&
                errors.back().type != ErrorType::MISSING_EXPRESSION)) {
            errors.emplace_back(
                "Unexpected token '" + token.value + "' found where an expression term (identifier or number) was expected.",
                token.line, ErrorType::MISSING_TERM, token.value, tokenTypeToString(token.type),
                "identifier or number",
                "Ensure expressions use valid variable names or numbers. Parentheses are not supported in this version."
            );
        }
        if (parsingSteps) (*parsingSteps).push_back("<- Exiting parseFactor (Unexpected Token)");
        return nullptr;
    }
    if (factorNode) factorNode->fromError = nodeHasError;
    if (parsingSteps) (*parsingSteps).push_back("<- Exiting parseFactor: " + factorNode->nodeType + (factorNode->value.empty() ? "" : " (" + factorNode->value + ")"));
    return factorNode;
}

ASTNode* parseIfStatement(const vector<Token>& tokens, size_t& index, vector<DetailedSyntaxError>& errors, vector<string>* parsingSteps) {
    int line = currentToken(tokens, index).line;
    if (parsingSteps) (*parsingSteps).push_back("-> Entering parseIfStatement at line " + to_string(line));
    bool nodeHasError = false;

    ASTNode* ifNode = new ASTNode("IfStatement", line);

    if (!match(tokens, index, TokenType::KEYWORD, errors, "if", "keyword 'if'", parsingSteps)) {
        if (parsingSteps) (*parsingSteps).push_back("Failed to match 'if'.");
        delete ifNode;
        if (parsingSteps) (*parsingSteps).push_back("<- Exiting parseIfStatement (Match Failed)");
        return nullptr;
    }
    if (parsingSteps) (*parsingSteps).push_back("Matched 'if'. Parsing Comparison.");
    int compExpectedLine = currentToken(tokens, index).line;
    ASTNode* condition = parseComparison(tokens, index, errors, parsingSteps);
    if (!condition) {
        if (parsingSteps) (*parsingSteps).push_back("Failed to parse Comparison after 'if'.");
        if (errors.empty() || errors.back().line < compExpectedLine ||
            (errors.back().line == compExpectedLine &&
                errors.back().type != ErrorType::MISSING_COMPARISON &&
                errors.back().type != ErrorType::MISSING_OPERATOR &&
                errors.back().type != ErrorType::MISSING_EXPRESSION &&
                errors.back().type != ErrorType::MISSING_TERM &&
                errors.back().type != ErrorType::INVALID_TOKEN)) {
            const Token& current = currentToken(tokens, index);
            errors.emplace_back(
                "Missing or invalid comparison after 'if' keyword.",
                compExpectedLine, ErrorType::MISSING_COMPARISON,
                current.value, tokenTypeToString(current.type),
                "comparison (e.g., x < 5, y = 0)",
                "Provide a valid comparison (using = or <) after 'if'."
            );
        }
        nodeHasError = true;
        if (parsingSteps) (*parsingSteps).push_back("Attempting synchronization to 'then', 'else', or 'end'.");
        synchronize(tokens, index, { "then", "else", "end" }, parsingSteps);
        if (parsingSteps) (*parsingSteps).push_back("Synchronization reached line " + to_string(currentToken(tokens, index).line) + ".");
    }
    else {
        if (parsingSteps) (*parsingSteps).push_back("Parsed Comparison after 'if'.");
        if (condition->fromError) nodeHasError = true;
        ifNode->children.push_back(condition);
    }
    int thenExpectedLine = currentToken(tokens, index).line;
    if (!match(tokens, index, TokenType::KEYWORD, errors, "then", "keyword 'then'", parsingSteps)) {
        if (parsingSteps) (*parsingSteps).push_back("Failed to match 'then'.");
        nodeHasError = true;
    }
    else {
        if (parsingSteps) (*parsingSteps).push_back("Matched 'then'. Parsing Then Block.");
    }
    set<string> thenRecovery = { "else", "end" };
    thenRecovery.insert(statementRecoverySet.begin(), statementRecoverySet.end());
    ASTNode* thenBlock = parseStatementSequence(tokens, index, errors, thenRecovery, parsingSteps);
    if (!thenBlock || thenBlock->fromError) {
        if (parsingSteps) (*parsingSteps).push_back("Failed to parse Then Block.");
        nodeHasError = true;
    }
    else {
        if (parsingSteps) (*parsingSteps).push_back("Parsed Then Block.");
    }
    if (thenBlock) {
        ifNode->children.push_back(thenBlock);
    }
    else {
        nodeHasError = true;
    }
    ASTNode* elseBlock = nullptr;
    if (currentToken(tokens, index).type == TokenType::KEYWORD && currentToken(tokens, index).value == "else") {
        if (parsingSteps) (*parsingSteps).push_back("Found 'else'. Parsing Else Block.");
        match(tokens, index, TokenType::KEYWORD, errors, "else", "keyword 'else'", parsingSteps);
        set<string> elseRecovery = { "end" };
        elseRecovery.insert(statementRecoverySet.begin(), statementRecoverySet.end());
        elseBlock = parseStatementSequence(tokens, index, errors, elseRecovery, parsingSteps);
        if (!elseBlock || elseBlock->fromError) {
            if (parsingSteps) (*parsingSteps).push_back("Failed to parse Else Block.");
            nodeHasError = true;
        }
        else {
            if (parsingSteps) (*parsingSteps).push_back("Parsed Else Block.");
        }
        if (elseBlock) {
            ifNode->children.push_back(elseBlock);
        }
        else {
            nodeHasError = true;
        }
    }
    else {
        if (parsingSteps) (*parsingSteps).push_back("No 'else' block found.");
    }
    int endExpectedLine = currentToken(tokens, index).line;
    if (!match(tokens, index, TokenType::KEYWORD, errors, "end", "keyword 'end'", parsingSteps)) {
        if (parsingSteps) (*parsingSteps).push_back("Failed to match 'end'.");
        nodeHasError = true;
    }
    else {
        if (parsingSteps) (*parsingSteps).push_back("Matched 'end'.");
    }
    ifNode->fromError = nodeHasError;
    ifNode->children.erase(remove(ifNode->children.begin(), ifNode->children.end(), nullptr), ifNode->children.end());
    if (parsingSteps) (*parsingSteps).push_back("<- Exiting parseIfStatement");
    return ifNode;
}

ASTNode* parseRepeatStatement(const vector<Token>& tokens, size_t& index, vector<DetailedSyntaxError>& errors, vector<string>* parsingSteps) {
    int line = currentToken(tokens, index).line;
    if (parsingSteps) (*parsingSteps).push_back("-> Entering parseRepeatStatement at line " + to_string(line));
    bool nodeHasError = false;
    ASTNode* repeatNode = new ASTNode("RepeatStatement", line);
    if (!match(tokens, index, TokenType::KEYWORD, errors, "repeat", "keyword 'repeat'", parsingSteps)) {
        if (parsingSteps) (*parsingSteps).push_back("Failed to match 'repeat'.");
        delete repeatNode;
        if (parsingSteps) (*parsingSteps).push_back("<- Exiting parseRepeatStatement (Match Failed)");
        return nullptr;
    }
    if (parsingSteps) (*parsingSteps).push_back("Matched 'repeat'. Parsing Repeat Body.");
    set<string> repeatRecovery = { "until" };
    repeatRecovery.insert(statementRecoverySet.begin(), statementRecoverySet.end());
    ASTNode* body = parseStatementSequence(tokens, index, errors, repeatRecovery, parsingSteps);
    if (!body || body->fromError) {
        if (parsingSteps) (*parsingSteps).push_back("Failed to parse Repeat Body.");
        nodeHasError = true;
        if (!body && currentToken(tokens, index).value != "until") {
            if (parsingSteps) (*parsingSteps).push_back("Attempting synchronization to 'until'.");
            synchronize(tokens, index, { "until" }, parsingSteps);
            if (parsingSteps) (*parsingSteps).push_back("Synchronization reached line " + to_string(currentToken(tokens, index).line) + ".");
        }
    }
    else {
        if (parsingSteps) (*parsingSteps).push_back("Parsed Repeat Body.");
    }
    if (body) {
        repeatNode->children.push_back(body);
    }
    else {
        nodeHasError = true;
    }
    int untilExpectedLine = currentToken(tokens, index).line;

    if (!match(tokens, index, TokenType::KEYWORD, errors, "until", "keyword 'until'", parsingSteps)) {
        if (parsingSteps) (*parsingSteps).push_back("Failed to match 'until'.");
        nodeHasError = true;
        if (parsingSteps) (*parsingSteps).push_back("Attempting synchronization after failed 'until' match.");
        synchronize(tokens, index, statementRecoverySet, parsingSteps);
        if (parsingSteps) (*parsingSteps).push_back("Synchronization reached line " + to_string(currentToken(tokens, index).line) + ".");
        repeatNode->fromError = nodeHasError;
        repeatNode->children.erase(remove(repeatNode->children.begin(), repeatNode->children.end(), nullptr), repeatNode->children.end());
        if (parsingSteps) (*parsingSteps).push_back("<- Exiting parseRepeatStatement (Missing Until)");
        return repeatNode;
    }
    if (parsingSteps) (*parsingSteps).push_back("Matched 'until'. Parsing Comparison.");
    int compExpectedLine = currentToken(tokens, index).line;
    ASTNode* condition = parseComparison(tokens, index, errors, parsingSteps);
    if (!condition) {
        if (parsingSteps) (*parsingSteps).push_back("Failed to parse Comparison after 'until'.");
        if (errors.empty() || errors.back().line < compExpectedLine ||
            (errors.back().line == compExpectedLine &&
                errors.back().type != ErrorType::MISSING_COMPARISON &&
                errors.back().type != ErrorType::MISSING_OPERATOR &&
                errors.back().type != ErrorType::MISSING_EXPRESSION &&
                errors.back().type != ErrorType::MISSING_TERM &&
                errors.back().type != ErrorType::INVALID_TOKEN)) {
            const Token& current = currentToken(tokens, index);
            errors.emplace_back(
                "Missing or invalid comparison after 'until' keyword.",
                compExpectedLine, ErrorType::MISSING_COMPARISON, current.value, tokenTypeToString(current.type),
                "comparison (e.g., x = 0)",
                "Provide a valid comparison (using = or <) after 'until'."
            );
        }
        nodeHasError = true;
    }
    else {
        if (parsingSteps) (*parsingSteps).push_back("Parsed Comparison after 'until'.");
        if (condition->fromError) nodeHasError = true;
        repeatNode->children.push_back(condition);
    }
    repeatNode->fromError = nodeHasError;
    repeatNode->children.erase(remove(repeatNode->children.begin(), repeatNode->children.end(), nullptr), repeatNode->children.end());
    if (parsingSteps) (*parsingSteps).push_back("<- Exiting parseRepeatStatement");
    return repeatNode;
}

ASTNode* parseComparison(const vector<Token>& tokens, size_t& index, vector<DetailedSyntaxError>& errors, vector<string>* parsingSteps) {
    int line = currentToken(tokens, index).line;

    if (parsingSteps) (*parsingSteps).push_back("-> Entering parseComparison at line " + to_string(line));
    bool nodeHasError = false;
    ASTNode* compNode = new ASTNode("Comparison", line);
    ASTNode* leftExpr = parseExpression(tokens, index, errors, parsingSteps);
    if (!leftExpr) {

        if (parsingSteps) (*parsingSteps).push_back("Failed to parse left Expression in Comparison.");

        if (errors.empty() || errors.back().line < line ||
            (errors.back().line == line &&
                errors.back().type != ErrorType::MISSING_EXPRESSION &&
                errors.back().type != ErrorType::MISSING_TERM &&
                errors.back().type != ErrorType::INVALID_TOKEN)) {
            const Token& current = currentToken(tokens, index);
            errors.emplace_back(
                "Missing left-hand side expression in comparison.",
                line, ErrorType::MISSING_EXPRESSION, current.value, tokenTypeToString(current.type),
                "expression (variable or number)",
                "Start the comparison with a variable or number."
            );
        }
        delete compNode;
        if (parsingSteps) (*parsingSteps).push_back("<- Exiting parseComparison (Failed Left Expr)");
        return nullptr;
    }
    if (parsingSteps) (*parsingSteps).push_back("Parsed left Expression in Comparison.");
    if (leftExpr->fromError) nodeHasError = true;
    compNode->children.push_back(leftExpr);

    const Token& opToken = currentToken(tokens, index);
    string opValue = opToken.value;

    if (opToken.type == TokenType::OPERATOR && (opValue == "=" || opValue == "<")) {
        if (parsingSteps) (*parsingSteps).push_back("Found comparison operator '" + opValue + "'.");
        if (!match(tokens, index, TokenType::OPERATOR, errors, opValue, "comparison operator ('=' or '<')", parsingSteps)) {
            if (parsingSteps) (*parsingSteps).push_back("Failed to match comparison operator.");
            nodeHasError = true;
            compNode->fromError = nodeHasError;
            if (parsingSteps) (*parsingSteps).push_back("<- Exiting parseComparison (Operator Match Failed)");
            return compNode;
        }
        if (parsingSteps) (*parsingSteps).push_back("Matched comparison operator '" + opValue + "'. Parsing right Expression.");
        compNode->children.push_back(new ASTNode("Operator", opToken.line, opValue));
    }
    else {
        if (parsingSteps) (*parsingSteps).push_back("Unexpected token '" + opToken.value + "' where comparison operator expected.");
        string expectedDesc = "comparison operator ('=' or '<')";
        string suggestion = "Insert '=' or '<' between the compared expressions.";
        if (opToken.type == TokenType::OPERATOR) {
            suggestion = "Replace operator '" + opValue + "' with '=' or '<'.";
        }
        else if (opToken.type == TokenType::IDENTIFIER || opToken.type == TokenType::NUMBER) {
            suggestion = "Insert comparison operator ('=' or '<') before '" + opValue + "'.";
        }
        if (errors.empty() || errors.back().line < opToken.line ||
            (errors.back().line == opToken.line &&
                errors.back().type != ErrorType::MISSING_OPERATOR &&
                errors.back().type != ErrorType::INVALID_TOKEN)) {
            errors.emplace_back(
                "Expected comparison operator ('=' or '<') but found '" + opToken.value + "'.",
                opToken.line, ErrorType::MISSING_OPERATOR, opToken.value, tokenTypeToString(opToken.type),
                expectedDesc, suggestion
            );
        }
        nodeHasError = true;
        compNode->fromError = nodeHasError;
        if (parsingSteps) (*parsingSteps).push_back("Attempting synchronization after failed comparison operator match.");
        synchronize(tokens, index, expressionRecoverySet, parsingSteps);
        if (parsingSteps) (*parsingSteps).push_back("Synchronization reached line " + to_string(currentToken(tokens, index).line) + ".");
        if (parsingSteps) (*parsingSteps).push_back("<- Exiting parseComparison (Missing Operator)");
        return compNode;
    }
    int rhsExpectedLine = currentToken(tokens, index).line;
    ASTNode* rightExpr = parseExpression(tokens, index, errors, parsingSteps);
    if (!rightExpr) {
        if (parsingSteps) (*parsingSteps).push_back("Failed to parse right Expression in Comparison.");
        if (errors.empty() || errors.back().line < rhsExpectedLine ||
            (errors.back().line == rhsExpectedLine &&
                errors.back().type != ErrorType::MISSING_EXPRESSION &&
                errors.back().type != ErrorType::MISSING_TERM &&
                errors.back().type != ErrorType::INVALID_TOKEN)) {
            const Token& current = currentToken(tokens, index);
            errors.emplace_back(
                "Missing right-hand side expression after comparison operator '" + opValue + "'.",
                opToken.line, ErrorType::MISSING_EXPRESSION, current.value, tokenTypeToString(current.type),
                "expression (variable or number)",
                "Provide a variable or number after the comparison operator '" + opValue + "'."
            );
        }
        nodeHasError = true;
    }
    else {
        if (parsingSteps) (*parsingSteps).push_back("Parsed right Expression in Comparison.");
        if (rightExpr->fromError) nodeHasError = true;
        compNode->children.push_back(rightExpr);
    }
    compNode->fromError = nodeHasError;
    if (parsingSteps) (*parsingSteps).push_back("<- Exiting parseComparison");
    return compNode;
}

ParseResult::~ParseResult() {
    delete astRoot;
    astRoot = nullptr;
}

ParseResult::ParseResult(ParseResult&& other) noexcept
    : astRoot(other.astRoot), errors(std::move(other.errors)), steps(std::move(other.steps)) {
    other.astRoot = nullptr;
}

ParseResult& ParseResult::operator=(ParseResult&& other) noexcept {
    if (this != &other) {
        delete astRoot;
        astRoot = other.astRoot;
        errors = std::move(other.errors);
        steps = std::move(other.steps);
        other.astRoot = nullptr;
    }
    return *this;
}

void parseTokens(const std::vector<Token>& tokens, ParseResult& result) {
    result.errors.clear();
    result.steps.clear();
    result.astRoot = nullptr;
    try {
        result.astRoot = parseProgram(tokens, result.errors, &result.steps);
    }
    catch (const std::exception& e) {
        result.errors.emplace_back("Unhandled C++ Exception during parsing: " + string(e.what()), 0, ErrorType::INTERNAL_ERROR);
    }
    catch (...) {
        result.errors.emplace_back("Unknown Unhandled Exception during parsing.", 0, ErrorType::INTERNAL_ERROR);
    }
}

//void printAST(const ASTNode* node, int indent) {
//    if (!node) {
//        for (int i = 0; i < indent; ++i) cout << "  ";
//        cout << "<null>" << endl;
//        return;
//    }
//    for (int i = 0; i < indent; ++i) cout << "  ";
//    cout << node->nodeType;
//    if (!node->value.empty()) cout << " (" << node->value << ")";
//    cout << " [Line: " << node->line << "]";
//    if (node->fromError) cout << " (Error/Incomplete)";
//    cout << endl;
//    if (node->nodeType == "IfStatement" && !node->children.empty()) {
//        if (node->children.size() > 0) {
//            for (int i = 0; i < indent + 1; ++i) cout << "  ";
//            cout << "Condition:" << endl;
//            printAST(node->children[0], indent + 2);
//        }
//        if (node->children.size() > 1) {
//            for (int i = 0; i < indent + 1; ++i) cout << "  ";
//            cout << "Then Block:" << endl;
//            printAST(node->children[1], indent + 2);
//        }
//        if (node->children.size() > 2) {
//            for (int i = 0; i < indent + 1; ++i) cout << "  ";
//            cout << "Else Block:" << endl;
//            printAST(node->children[2], indent + 2);
//        }
//    }
//    else {
//        for (const ASTNode* child : node->children) {
//            printAST(child, indent + 1);
//        }
//    }
//}