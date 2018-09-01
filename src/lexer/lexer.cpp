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
        {'!', std::bind(&Lexer::notEqual, std::placeholders::_1, std::placeholders::_2)},
        {'!', std::bind(&Lexer::bang, std::placeholders::_1, std::placeholders::_2)},

        // question
        {'?', std::bind(&Lexer::question, std::placeholders::_1, std::placeholders::_2)},

        // period/spread
        {'.', std::bind(&Lexer::period, std::placeholders::_1, std::placeholders::_2)},

        // block comment, line comment, slash
        {'/', std::bind(&Lexer::blockComment, std::placeholders::_1, std::placeholders::_2)},
        {'/', std::bind(&Lexer::lineComment, std::placeholders::_1, std::placeholders::_2)},
        {'/', std::bind(&Lexer::slash, std::placeholders::_1, std::placeholders::_2)},

        // tilde
        {'~', std::bind(&Lexer::tilde, std::placeholders::_1, std::placeholders::_2)},

        // colon
        {':', std::bind(&Lexer::colon, std::placeholders::_1, std::placeholders::_2)},

        // percent/number literal
        {'%', std::bind(&Lexer::percent, std::placeholders::_1, std::placeholders::_2)},

        // asterisk
        {'*', std::bind(&Lexer::asterisk, std::placeholders::_1, std::placeholders::_2)},

        // ampersand
        {'&', std::bind(&Lexer::ampersand, std::placeholders::_1, std::placeholders::_2)},

        // equal, assignOp
        {'=', std::bind(&Lexer::equal, std::placeholders::_1, std::placeholders::_2)},
        {'=', std::bind(&Lexer::assignOp, std::placeholders::_1, std::placeholders::_2)},

        // gt, ge
        {'>', std::bind(&Lexer::greaterThanEqual, std::placeholders::_1, std::placeholders::_2)},
        {'>', std::bind(&Lexer::greaterThan, std::placeholders::_1, std::placeholders::_2)},
        
        // lt, le
        {'<', std::bind(&Lexer::lessThanEqual, std::placeholders::_1, std::placeholders::_2)},
        {'<', std::bind(&Lexer::lessThan, std::placeholders::_1, std::placeholders::_2)},

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
            &_source,
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

    /*
     * updateLocation should only be called, if we are committing to
     * the updatedLocation. There is no easy way to revert the location.
     */
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
        if (_currentPosition == code.length()) {
            return '\0';
        }
        auto character = code[_currentPosition];
        if (advancePosition) {
            _currentPosition++;
        }
        return character;
    }

    bool Lexer::plus(Token& token) {
        return readSingleLineToken(token, TokenType::PLUS);
    }

    bool Lexer::bang(Token& token) {
        return readSingleLineToken(token, TokenType::BANG);
    }

    bool Lexer::pipe(Token& token) {
        return readSingleLineToken(token, TokenType::PIPE);
    }

    bool Lexer::minus(Token& token) {
        return readSingleLineToken(token, TokenType::MINUS);
    }

    bool Lexer::slash(Token& token) {
        return readSingleLineToken(token, TokenType::SLASH);
    }

    bool Lexer::caret(Token& token) {
        return readSingleLineToken(token, TokenType::CARET);
    }

    bool Lexer::tilde(Token& token) {
        return readSingleLineToken(token, TokenType::TILDE);
    }

    bool Lexer::colon(Token& token) {
        return readSingleLineToken(token, TokenType::COLON);
    }

    bool Lexer::comma(Token& token) {
        return readSingleLineToken(token, TokenType::COMMA);
    }

    bool Lexer::period(Token& token) {
        return readSingleLineToken(token, TokenType::PERIOD);
    }

    bool Lexer::percent(Token& token) {
        return readSingleLineToken(token, TokenType::PERCENT);
    }

    bool Lexer::question(Token& token) {
        return readSingleLineToken(token, TokenType::QUESTION);
    }

    bool Lexer::asterisk(Token& token) {
        return readSingleLineToken(token, TokenType::ASTERISK);
    }

    bool Lexer::ampersand(Token& token) {
        return readSingleLineToken(token, TokenType::AMPERSAND);
    }

    bool Lexer::assignOp(Token& token) {
        return readSingleLineToken(token, TokenType::ASSIGN_OP);
    }

    bool Lexer::equal(Token& token) {
        auto code = _source.code();
        auto equalStart = _currentPosition;
        auto startLocation = _sourceLocation;
        // ignore first =
        read();
        if (read() != '=') {
            _currentPosition = equalStart;
            return false;
        }

        auto equalEnd = _currentPosition;
        updateLocation();
        auto endLocation = _sourceLocation;
        token.type = TokenType::EQ_OP;
        token.start = startLocation;
        token.end = endLocation;
        token.value = code.substr(equalStart, equalEnd - equalStart);
        return true;
    }

    bool Lexer::notEqual(Token& token) {
        auto code = _source.code();
        auto equalStart = _currentPosition;
        auto startLocation = _sourceLocation;
        // ignore first !
        read();
        if (read() != '=') {
            _currentPosition = equalStart;
            return false;
        }

        auto equalEnd = _currentPosition;
        updateLocation();
        auto endLocation = _sourceLocation;
        token.type = TokenType::NE_OP;
        token.start = startLocation;
        token.end = endLocation;
        token.value = code.substr(equalStart, equalEnd - equalStart);
        return true;
    }

    bool Lexer::lessThan(Token& token) {
        return readSingleLineToken(token, TokenType::LT_OP);
    }

    bool Lexer::greaterThan(Token& token) {
        return readSingleLineToken(token, TokenType::GT_OP);
    }

    bool Lexer::lessThanEqual(Token& token) {
        auto code = _source.code();
        auto lessThanEqualStart = _currentPosition;
        auto startLocation = _sourceLocation;
        // ignore first <
        read();
        if (read() != '=') {
            _currentPosition = lessThanEqualStart;
            return false;
        }

        auto lessThanEqualEnd = _currentPosition;
        updateLocation();
        auto endLocation = _sourceLocation;
        token.type = TokenType::LE_OP;
        token.start = startLocation;
        token.end = endLocation;
        token.value = code.substr(lessThanEqualStart, lessThanEqualEnd - lessThanEqualStart);
        return true;
    }

    bool Lexer::greaterThanEqual(Token& token) {
                auto code = _source.code();
        auto greaterThanEqualStart = _currentPosition;
        auto startLocation = _sourceLocation;
        // ignore first >
        read();
        if (read() != '=') {
            _currentPosition = greaterThanEqualStart;
            return false;
        }

        auto greaterThanEqualEnd = _currentPosition;
        updateLocation();
        auto endLocation = _sourceLocation;
        token.type = TokenType::GE_OP;
        token.start = startLocation;
        token.end = endLocation;
        token.value = code.substr(greaterThanEqualStart, greaterThanEqualEnd - greaterThanEqualStart);
        return true;
    }
    
    bool Lexer::leftCurlyBrace(Token& token) {
        return readSingleLineToken(token, TokenType::LEFT_CURLY_BRACE);
    }

    bool Lexer::rightCurlyBrace(Token& token) {
        return readSingleLineToken(token, TokenType::RIGHT_CURLY_BRACE);
    }

    bool Lexer::leftParenthesis(Token& token) {
        return readSingleLineToken(token, TokenType::LEFT_PAR);
    }

    bool Lexer::rightParenthesis(Token& token) {
        return readSingleLineToken(token, TokenType::RIGHT_PAR);
    }

    bool Lexer::blockComment(Token& token) {
        auto code = _source.code();
        auto commentStart = _currentPosition;
        auto startLocation = _sourceLocation;

        // ignore the /
        read();
        if (read() != '*') {
            _currentPosition = commentStart;
            return false;
        }
        
        while(_currentPosition < code.length() - 1) {
            while(read() != '*') {
                if (_currentPosition == code.length() - 1) {
                    _currentPosition = commentStart;
                    return false;
                }
            }

            if (read() == '/') {
                auto commentEnd = _currentPosition;
                updateLocation();
                auto endLocation = _sourceLocation;
                token.type = TokenType::BLOCK_COMMENT;
                token.start = startLocation;
                token.end = endLocation;
                token.value = code.substr(commentStart, commentEnd - commentStart);
                return true;
            }
        }

        _currentPosition = commentStart;
        return false;
    }

    bool Lexer::lineComment(Token& token) {
        auto code = _source.code();
        auto commentStart = _currentPosition;
        auto startLocation = _sourceLocation;

        // ignore first /
        read();
        if (read() != '/') {
            _currentPosition = commentStart;
            return false;
        }

        // read while we have not reached the end of the line or the end of the file
        while(read(false) != '\n' && _currentPosition < code.length()) {
            _currentPosition++;
        }

        auto commentEnd = _currentPosition;
        updateLocation();
        auto endLocation = _sourceLocation;
        token.type = TokenType::LINE_COMMENT;
        token.start = startLocation;
        token.end = endLocation;
        token.value = code.substr(commentStart, commentEnd - commentStart);
        return true;
    }


}