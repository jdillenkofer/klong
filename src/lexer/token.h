#pragma once

#include <string>
#include "../common/source_location.h"

namespace klong {
    enum class TokenType {
        END_OF_FILE,
        NONE,
        ERROR,
        
        // Comments
        LINE_COMMENT,
        BLOCK_COMMENT,

        // Functions
        FUN,
        PRINT,
        RETURN,

        // Control flow
        IF,
        ELSE,
        WHILE,
        FOR,
        DO,
        
        // Declarators
        LET,
        CONST,
        
        PLUS,
        BANG,
        PIPE,
        MINUS,
        HASH,
        AT_SIGN,
        DOLLAR,
        BACKSLASH,
        BACKQUOTE,
        SLASH,
        CARET,
        TILDE,
        SEMICOLON,
        COLON,
        COMMA,
        PERIOD,
        PERCENT,
        QUESTION,
        ASTERISK,
        AMPERSAND,
        ASSIGN_OP,
        EQ_OP,
        NE_OP,
        LT_OP,
        GT_OP,
        LE_OP,
        GE_OP,
        LEFT_CURLY_BRACE,
        RIGHT_CURLY_BRACE,
        LEFT_PAR,
        RIGHT_PAR,
        LEFT_SQUARED_BRACKET,
        RIGHT_SQUARED_BRACKET,

        IDENTIFIER,

        CHARACTER_LITERAL,
        NUMBER_LITERAL,
        STRING_LITERAL,
        TRUE_KEYWORD,
        FALSE_KEYWORD,

        // Types
        STRING,
        BOOL,
        I8,
        I16,
        I32,
        I64,
        U8,
        U16,
        U32,
        U64,
        F32,
        F64,
    };

    enum class NumberType {
        NONE,
        INTEGER,
        FLOAT
    };

    struct Token {
        Token() = default;

        SourceLocation start;
        SourceLocation end;
        TokenType type = TokenType::NONE;
        std::string value = "";
        NumberType numberType = NumberType::NONE;
        uint8_t radix = 10;
    };
}