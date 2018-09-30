#pragma once

#include "token.h"

namespace klong {
    class LexerException : public std::exception {
    public:
        LexerException(Token token, std::string message):
            _token(token), _message(message) {
        }

        const char* what () const throw () {
            return _message.c_str();
        }

    private:
        Token _token;
        std::string _message;
    };

    class ILexer {
    public:
        virtual ~ILexer() = default;
        virtual bool hasNext() const = 0;
        virtual Token next() = 0;
    };
}