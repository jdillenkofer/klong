#include "lexer.h"
namespace klong {
    bool Lexer::hasNext() const {
        auto position = _currentPosition;
        auto code = _source.code();
        skipWhitespace(position);
        return position < code.length();
    }
    
    Token Lexer::next() {
        return determineNextToken(_currentPosition, _sourceLocation);
    }

    Token Lexer::peek() const {
        size_t peekPosition = _currentPosition;
        SourceLocation location(_sourceLocation);
        return determineNextToken(peekPosition, location);
    }

    Token Lexer::determineNextToken(size_t& position, SourceLocation& location) const {
        skipWhitespace(position);        
        updateLocation(position, location);
        // implement lexing
        Token token = {
            .type = TokenType::END_OF_FILE,
            .location = location,
            .value = std::string(1, _source.code()[position])
        };
        position++;
        return token;
    }

    void Lexer::updateLocation(size_t position, SourceLocation& location) const {
        const auto code = _source.code();
        for (auto i = location.charPos(); i < position; i++) {
            const auto c = code[i];
            switch(c) {
                case '\n':
                    location.incLine();
                    break;
                default:
                    location.incCol();
            }
        }
    }

    void Lexer::skipWhitespace(size_t& position) const {
        const auto code = _source.code();
        while (isWhitespace(code[position])) {
            position++;
        }
    }
}