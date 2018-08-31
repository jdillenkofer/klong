#include "lexer.h"
namespace klong {
    bool Lexer::hasNext() const {
        return false;
    }
    
    Token Lexer::next() {
        return {
            .type = TokenType::END_OF_FILE,
            .location = _currentLocation
        };
    }

    Token Lexer::peek() const {
        return {
            .type = TokenType::END_OF_FILE,
            .location = _currentLocation
        };
    }

    void Lexer::reset() {

    }
}