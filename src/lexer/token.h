#pragma once

#include <string>
#include <climits>
#include <cfloat>
#include "../common/source_range.h"

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
        
        ARROW,
        PLUS,
        BANG,
        PIPE,
        OR,
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
        AND,
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
        VOID,
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
        INT,
        UINT,
        FLOAT
    };

    enum class NumberConversionResult {
        OVERFLOW,
        UNDERFLOW,
        INCONVERTIBLE,
        OK
    };

    struct Token {
        Token() = default;

        NumberConversionResult parse(int64_t& out) {
            const char* str = value.c_str();
            char* endPtr;
            errno = 0;
            out = strtoll(str, &endPtr, radix);
            if (errno == ERANGE) {
                return out == LONG_MAX ? NumberConversionResult::OVERFLOW : NumberConversionResult::UNDERFLOW;
            }
            if (*endPtr != '\0') {
                return NumberConversionResult::INCONVERTIBLE;
            }
            return NumberConversionResult::OK;
        }

        NumberConversionResult parse(uint64_t& out) {
            const char* str = value.c_str();
            char* endPtr;
            errno = 0;
            out = strtoull(str, &endPtr, radix);
            if (errno == ERANGE) {
                return out == ULONG_MAX ? NumberConversionResult::OVERFLOW : NumberConversionResult::UNDERFLOW;
            }
            if (*endPtr != '\0') {
                return NumberConversionResult::INCONVERTIBLE;
            }
            return NumberConversionResult::OK;
        }

        NumberConversionResult parse(double& out) {
            const char* str = value.c_str();
            char* endPtr;
            errno = 0;
            out = strtod(str, &endPtr);
            if (errno == ERANGE) {
                return out == DBL_MAX ? NumberConversionResult::OVERFLOW : NumberConversionResult::UNDERFLOW;
            }
            if (*endPtr != '\0') {
                return NumberConversionResult::INCONVERTIBLE;
            }
            return NumberConversionResult::OK;
        }

        SourceRange sourceRange;
        TokenType type = TokenType::NONE;
        std::string value = "";
        NumberType numberType = NumberType::NONE;
        uint8_t radix = 10;
    };
}