#pragma once

#include "common/result.h"
#include "lexer/token.h"
#include "ast/module.h"

namespace klong {
    class ParseException : public std::exception {
    public:
        ParseException(SourceRange sourceRange, std::string message):
            _sourceRange(sourceRange), _message(std::move(message)) {
        }

        static ParseException from(Token token, const std::string& message) {
            return ParseException(token.sourceRange, message);
        }

        SourceRange sourceRange() const {
            return _sourceRange;
        }

        const char* what () const noexcept override {
            return _message.c_str();
        }

    private:
        SourceRange _sourceRange;
        std::string _message;
    };

    class IParser {
    public:
        virtual ~IParser() = default;
        virtual Result<ModulePtr, ParseException> parse() = 0;
    };
}