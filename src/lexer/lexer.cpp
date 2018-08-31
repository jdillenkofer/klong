#include "lexer.h"

namespace klong {
    std::multimap<char, Lexer::LexerCaseCallable> Lexer::cases = {
        // plus
        {'+', std::bind(&Lexer::plus, std::placeholders::_1, std::placeholders::_2)},
        // comma
        {',', std::bind(&Lexer::comma, std::placeholders::_1, std::placeholders::_2)},

        // caret
        {'^', std::bind(&Lexer::caret, std::placeholders::_1, std::placeholders::_2)},

        // bang
        {'!', std::bind(&Lexer::bang, std::placeholders::_1, std::placeholders::_2)},

        // question
        {'?', std::bind(&Lexer::question, std::placeholders::_1, std::placeholders::_2)},

        // period/spread
        {'.', std::bind(&Lexer::period, std::placeholders::_1, std::placeholders::_2)},

        // tilde
        {'~', std::bind(&Lexer::tilde, std::placeholders::_1, std::placeholders::_2)},

        // assignment, scope operator, colon
        {':', std::bind(&Lexer::colon, std::placeholders::_1, std::placeholders::_2)},

        // percent/number literal
        {'%', std::bind(&Lexer::percent, std::placeholders::_1, std::placeholders::_2)},

        // asterisk
        {'*', std::bind(&Lexer::asterisk, std::placeholders::_1, std::placeholders::_2)},

        // ampersand
        {'&', std::bind(&Lexer::ampersand, std::placeholders::_1, std::placeholders::_2)},

        // pipe
        {'|', std::bind(&Lexer::pipe, std::placeholders::_1, std::placeholders::_2)},

        // block-braces
        {'{', std::bind(&Lexer::leftCurlyBrace, std::placeholders::_1, std::placeholders::_2)},
        {'}', std::bind(&Lexer::rightCurlyBrace, std::placeholders::_1, std::placeholders::_2)},

        // parens
        {'(', std::bind(&Lexer::leftParenthesis, std::placeholders::_1, std::placeholders::_2)},
        {')', std::bind(&Lexer::rightParenthesis, std::placeholders::_1, std::placeholders::_2)},
    };

    bool Lexer::hasNext() const {
        auto position = _currentPosition;
        auto code = _source.code();
        skipWhitespace(position);
        return position < code.length();
    }
    
    Token Lexer::next() {
        skipWhitespace(_currentPosition);
        updateLocation();

        auto ch = read(false);
        Token token {
            &_source
        };

        auto case_range = cases.equal_range(ch);
        bool hasFoundMatchingCase = false;
        for (auto it = case_range.first; it != case_range.second; ++it) {
            if (it->second(this, token)) {
                hasFoundMatchingCase = true;
                break;
            }
        }

        if (!hasFoundMatchingCase) {
            _currentPosition++;
            updateLocation();
        }

        return token;
    }

    void Lexer::updateLocation() {
        const auto code = _source.code();
        for (auto i = _sourceLocation.charPos(); i < _currentPosition; i++) {
            const auto c = code[i];
            switch(c) {
                case '\n':
                    _sourceLocation.incLine();
                    break;
                default:
                    _sourceLocation.incCol();
            }
        }
    }

    void Lexer::skipWhitespace(size_t& position) const {
        const auto code = _source.code();
        while (isWhitespace(code[position])) {
            position++;
        }
    }

    char Lexer::read(bool advancePosition) {
        const auto code = _source.code();
        auto character = code[_currentPosition]; 
        if (advancePosition) {
            _currentPosition++;
        }
        return character;
    }

    bool Lexer::plus(Token& token) {
        auto c = read();
        token.type = TokenType::PLUS;
        token.location = _sourceLocation;
        token.value = std::string(1, c);
        return true;
    }

    bool Lexer::bang(Token& token) {
        auto c = read();
        token.type = TokenType::BANG;
        token.location = _sourceLocation;
        token.value = std::string(1, c);
        return true;
    }

    bool Lexer::pipe(Token& token) {
        auto c = read();
        token.type = TokenType::PIPE;
        token.location = _sourceLocation;
        token.value = std::string(1, c);
        return true;
    }

    bool Lexer::minus(Token& token) {
        auto c = read();
        token.type = TokenType::MINUS;
        token.location = _sourceLocation;
        token.value = std::string(1, c);
        return true;
    }

    bool Lexer::slash(Token& token) {
        auto c = read();
        token.type = TokenType::SLASH;
        token.location = _sourceLocation;
        token.value = std::string(1, c);
        return true;
    }

    bool Lexer::caret(Token& token) {
        auto c = read();
        token.type = TokenType::CARET;
        token.location = _sourceLocation;
        token.value = std::string(1, c);
        return true;
    }

    bool Lexer::tilde(Token& token) {
        auto c = read();
        token.type = TokenType::TILDE;
        token.location = _sourceLocation;
        token.value = std::string(1, c);
        return true;
    }

    bool Lexer::colon(Token& token) {
        auto c = read();
        token.type = TokenType::COLON;
        token.location = _sourceLocation;
        token.value = std::string(1, c);
        return true;
    }

    bool Lexer::comma(Token& token) {
        auto c = read();
        token.type = TokenType::COMMA;
        token.location = _sourceLocation;
        token.value = std::string(1, c);
        return true;
    }

    bool Lexer::assignOp(Token& token) {
        return false;
    }

    bool Lexer::period(Token& token) {
        auto c = read();
        token.type = TokenType::PERIOD;
        token.location = _sourceLocation;
        token.value = std::string(1, c);
        return true;
    }

    bool Lexer::percent(Token& token) {
        auto c = read();
        token.type = TokenType::PERCENT;
        token.location = _sourceLocation;
        token.value = std::string(1, c);
        return true;
    }

    bool Lexer::question(Token& token) {
        auto c = read();
        token.type = TokenType::QUESTION;
        token.location = _sourceLocation;
        token.value = std::string(1, c);
        return true;
    }

    bool Lexer::asterisk(Token& token) {
        auto c = read();
        token.type = TokenType::ASTERISK;
        token.location = _sourceLocation;
        token.value = std::string(1, c);
        return true;
    }

    bool Lexer::ampersand(Token& token) {
        auto c = read();
        token.type = TokenType::AMPERSAND;
        token.location = _sourceLocation;
        token.value = std::string(1, c);
        return true;
    }

    bool Lexer::leftCurlyBrace(Token& token) {
        auto c = read();
        token.type = TokenType::LEFT_CURLY_BRACE;
        token.location = _sourceLocation;
        token.value = std::string(1, c);
        return true;
    }

    bool Lexer::rightCurlyBrace(Token& token) {
        auto c = read();
        token.type = TokenType::RIGHT_CURLY_BRACE;
        token.location = _sourceLocation;
        token.value = std::string(1, c);
        return true;
    }

    bool Lexer::leftParenthesis(Token& token) {
        auto c = read();
        token.type = TokenType::LEFT_PAR;
        token.location = _sourceLocation;
        token.value = std::string(1, c);
        return true;
    }

    bool Lexer::rightParenthesis(Token& token) {
        auto c = read();
        token.type = TokenType::RIGHT_PAR;
        token.location = _sourceLocation;
        token.value = std::string(1, c);
        return true;
    }


}