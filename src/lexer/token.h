#pragma once

#include <string>
#include "../common/source_location.h"

namespace klong {
    enum class TokenType {
        END_OF_FILE,
        NONE,
        
        // Comments
        LINE_COMMENT,
        BLOCK_COMMENT,

        // Functions
        FUN,
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
        
        IDENTIFIER,
        
        PLUS,
        BANG,
        PIPE,
        MINUS,
        SLASH,
        CARET,
        TILDE,
        COLON,
        COMMA,
        ASSIGN_OP,
        PERIOD,
        PERCENT,
        QUESTION,
        ASTERISK,
        AMPERSAND,
        LT_OP,
        GT_OP,
        LE_OP,
        GE_OP,
        LEFT_CURLY_BRACE,
        RIGHT_CURLY_BRACE,
        LEFT_PAR,
        RIGHT_PAR,

        CHARACTER_LITERAL,
        NUMBER_LITERAL,
        STRING_LITERAL,
        BOOL_LITERAL,

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
        None,
        Integer,
        Float
    };

    struct Token {
        TokenType type = TokenType::NONE;
        SourceLocation location;
        std::string value;
        NumberType numberType = NumberType::None;
        uint8_t radix = 10;
    };
}