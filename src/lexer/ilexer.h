#pragma once

#include "token.h"
#include "lexer_memento.h"

namespace klong {
    class LexerException : public std::exception {
    public:
        LexerException(Token token, std::string message):
            _token(std::move(token)), _message(std::move(message)) {
        }

        const char* what() const noexcept {
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
        virtual LexerMemento saveToMemento() = 0;
        virtual void loadFromMemento(LexerMemento& memento) = 0;
    };
}