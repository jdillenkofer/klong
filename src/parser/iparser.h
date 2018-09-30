#pragma once

#include "../lexer/token.h"
#include "stmt.h"

namespace klong {
    class ParseException : public std::exception {
    public:
        ParseException(SourceRange sourceRange, std::string message):
            _sourceRange(sourceRange), _message(message) {
        }

        static ParseException from(Token token, const std::string& message) {
            return ParseException(token.sourceRange, message);
        }

        SourceRange sourceRange() const {
            return _sourceRange;
        }

        const char* what () const throw () {
            return _message.c_str();
        }

    private:
        SourceRange _sourceRange;
        std::string _message;
    };

    class IParser {
    public:
        virtual ~IParser() = default;
        virtual std::vector<StmtPtr> parse();
    };
}