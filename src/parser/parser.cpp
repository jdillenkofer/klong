#include "parser.h"

#include <exception>

namespace klong {
    std::vector<StmtPtr> Parser::parse() {
        std::vector<StmtPtr> statements;
        return statements;
    }

    Token Parser::consume(TokenType type, std::string errorMessage) {
        if (check(type)) {
            return advance();
        }

        throw ParseException(peek(), errorMessage);
    }

    bool Parser::check(TokenType type) {
        if(isAtEnd()) {
            return false;
        }
        return peek().type == type;
    }

    Token Parser::peek() {
        return _current;
    }

    Token Parser::previous() {
        return _previous;
    }

    Token Parser::advance() {
        if(!isAtEnd()) {
            _previous = _current;
            _current = _lexer->next();
        }
        return previous();
    }

    bool Parser::isAtEnd() {
        return peek().type == TokenType::END_OF_FILE;
    }
}