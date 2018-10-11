#include "lexer.h"

namespace klong {
    std::multimap<char, Lexer::LexerCaseCallable> Lexer::cases = {
        // plus
        {'+', std::bind(&Lexer::plus, std::placeholders::_1, std::placeholders::_2)},
        
        // arrow, minus
        {'-', std::bind(&Lexer::arrow, std::placeholders::_1, std::placeholders::_2)},
        {'-', std::bind(&Lexer::minus, std::placeholders::_1, std::placeholders::_2)},
        
        // comma
        {',', std::bind(&Lexer::comma, std::placeholders::_1, std::placeholders::_2)},

        // hash
        {'#', std::bind(&Lexer::hash, std::placeholders::_1, std::placeholders::_2)},
        
        // atSign
        {'@', std::bind(&Lexer::atSign, std::placeholders::_1, std::placeholders::_2)},

        // dollar
        {'$', std::bind(&Lexer::dollar, std::placeholders::_1, std::placeholders::_2)},

        // backslash
        {'\\', std::bind(&Lexer::backslash, std::placeholders::_1, std::placeholders::_2)},

        // backquote
        {'`', std::bind(&Lexer::backquote, std::placeholders::_1, std::placeholders::_2)},

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

        // semicolon
        {';', std::bind(&Lexer::semicolon, std::placeholders::_1, std::placeholders::_2)},

        // colon
        {':', std::bind(&Lexer::colon, std::placeholders::_1, std::placeholders::_2)},

        // percent
        {'%', std::bind(&Lexer::percent, std::placeholders::_1, std::placeholders::_2)},

        // asterisk
        {'*', std::bind(&Lexer::asterisk, std::placeholders::_1, std::placeholders::_2)},

        // ampersand
		{'&', std::bind(&Lexer::andOp, std::placeholders::_1, std::placeholders::_2) },
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

        // pipe, or
		{'|', std::bind(&Lexer::orOp, std::placeholders::_1, std::placeholders::_2) },
        {'|', std::bind(&Lexer::pipe, std::placeholders::_1, std::placeholders::_2)},

        // block-braces
        {'{', std::bind(&Lexer::leftCurlyBrace, std::placeholders::_1, std::placeholders::_2)},
        {'}', std::bind(&Lexer::rightCurlyBrace, std::placeholders::_1, std::placeholders::_2)},

        // parens
        {'(', std::bind(&Lexer::leftParenthesis, std::placeholders::_1, std::placeholders::_2)},
        {')', std::bind(&Lexer::rightParenthesis, std::placeholders::_1, std::placeholders::_2)},

        // squared-brackets
        {'[', std::bind(&Lexer::leftSquaredBracket, std::placeholders::_1, std::placeholders::_2)},
        {']', std::bind(&Lexer::rightSquaredBracket, std::placeholders::_1, std::placeholders::_2)},

        // bitshifts
        {'l', std::bind(&Lexer::lslOp, std::placeholders::_1, std::placeholders::_2)},
        {'l', std::bind(&Lexer::lsrOp, std::placeholders::_1, std::placeholders::_2)},
        {'a', std::bind(&Lexer::asrOp, std::placeholders::_1, std::placeholders::_2)},

        // modifier
        {'p', std::bind(&Lexer::pubKeyword, std::placeholders::_1, std::placeholders::_2)},
        {'e', std::bind(&Lexer::externKeyword, std::placeholders::_1, std::placeholders::_2)},

        // function
        {'f', std::bind(&Lexer::funKeyword, std::placeholders::_1, std::placeholders::_2)},
        {'s', std::bind(&Lexer::sizeOfKeyword, std::placeholders::_1, std::placeholders::_2)},
        {'r', std::bind(&Lexer::returnKeyword, std::placeholders::_1, std::placeholders::_2)},

        // control flow keyword
        {'i', std::bind(&Lexer::ifKeyword, std::placeholders::_1, std::placeholders::_2)},
        {'e', std::bind(&Lexer::elseKeyword, std::placeholders::_1, std::placeholders::_2)},
        {'w', std::bind(&Lexer::whileKeyword, std::placeholders::_1, std::placeholders::_2)},
        {'f', std::bind(&Lexer::forKeyword, std::placeholders::_1, std::placeholders::_2)},
        {'d', std::bind(&Lexer::doKeyword, std::placeholders::_1, std::placeholders::_2)},

        // let and const keyword
        {'l', std::bind(&Lexer::letKeyword, std::placeholders::_1, std::placeholders::_2)},
        {'c', std::bind(&Lexer::constKeyword, std::placeholders::_1, std::placeholders::_2)},

        // types
        {'p', std::bind(&Lexer::ptrType, std::placeholders::_1, std::placeholders::_2)},
        {'v', std::bind(&Lexer::voidType, std::placeholders::_1, std::placeholders::_2)},
        {'s', std::bind(&Lexer::stringType, std::placeholders::_1, std::placeholders::_2)},
        {'b', std::bind(&Lexer::boolType, std::placeholders::_1, std::placeholders::_2)},
        {'i', std::bind(&Lexer::i8Type, std::placeholders::_1, std::placeholders::_2)},
        {'i', std::bind(&Lexer::i16Type, std::placeholders::_1, std::placeholders::_2)},
        {'i', std::bind(&Lexer::i32Type, std::placeholders::_1, std::placeholders::_2)},
        {'i', std::bind(&Lexer::i64Type, std::placeholders::_1, std::placeholders::_2)},
        {'u', std::bind(&Lexer::u8Type, std::placeholders::_1, std::placeholders::_2)},
        {'u', std::bind(&Lexer::u16Type, std::placeholders::_1, std::placeholders::_2)},
        {'u', std::bind(&Lexer::u32Type, std::placeholders::_1, std::placeholders::_2)},
        {'u', std::bind(&Lexer::u64Type, std::placeholders::_1, std::placeholders::_2)},
        {'f', std::bind(&Lexer::f32Type, std::placeholders::_1, std::placeholders::_2)},
        {'f', std::bind(&Lexer::f64Type, std::placeholders::_1, std::placeholders::_2)},

        // true and false keywords
        {'t', std::bind(&Lexer::trueKeyword, std::placeholders::_1, std::placeholders::_2)},
        {'f', std::bind(&Lexer::falseKeyword, std::placeholders::_1, std::placeholders::_2)},

        // identifier
        {'_', std::bind(&Lexer::identifier, std::placeholders::_1, std::placeholders::_2)},
        {'a', std::bind(&Lexer::identifier, std::placeholders::_1, std::placeholders::_2)},
        {'b', std::bind(&Lexer::identifier, std::placeholders::_1, std::placeholders::_2)},
        {'c', std::bind(&Lexer::identifier, std::placeholders::_1, std::placeholders::_2)},
        {'d', std::bind(&Lexer::identifier, std::placeholders::_1, std::placeholders::_2)},
        {'e', std::bind(&Lexer::identifier, std::placeholders::_1, std::placeholders::_2)},
        {'f', std::bind(&Lexer::identifier, std::placeholders::_1, std::placeholders::_2)},
        {'g', std::bind(&Lexer::identifier, std::placeholders::_1, std::placeholders::_2)},
        {'h', std::bind(&Lexer::identifier, std::placeholders::_1, std::placeholders::_2)},
        {'i', std::bind(&Lexer::identifier, std::placeholders::_1, std::placeholders::_2)},
        {'j', std::bind(&Lexer::identifier, std::placeholders::_1, std::placeholders::_2)},
        {'k', std::bind(&Lexer::identifier, std::placeholders::_1, std::placeholders::_2)},
        {'l', std::bind(&Lexer::identifier, std::placeholders::_1, std::placeholders::_2)},
        {'m', std::bind(&Lexer::identifier, std::placeholders::_1, std::placeholders::_2)},
        {'n', std::bind(&Lexer::identifier, std::placeholders::_1, std::placeholders::_2)},
        {'o', std::bind(&Lexer::identifier, std::placeholders::_1, std::placeholders::_2)},
        {'p', std::bind(&Lexer::identifier, std::placeholders::_1, std::placeholders::_2)},
        {'q', std::bind(&Lexer::identifier, std::placeholders::_1, std::placeholders::_2)},
        {'r', std::bind(&Lexer::identifier, std::placeholders::_1, std::placeholders::_2)},
        {'s', std::bind(&Lexer::identifier, std::placeholders::_1, std::placeholders::_2)},
        {'t', std::bind(&Lexer::identifier, std::placeholders::_1, std::placeholders::_2)},
        {'u', std::bind(&Lexer::identifier, std::placeholders::_1, std::placeholders::_2)},
        {'v', std::bind(&Lexer::identifier, std::placeholders::_1, std::placeholders::_2)},
        {'w', std::bind(&Lexer::identifier, std::placeholders::_1, std::placeholders::_2)},
        {'x', std::bind(&Lexer::identifier, std::placeholders::_1, std::placeholders::_2)},
        {'y', std::bind(&Lexer::identifier, std::placeholders::_1, std::placeholders::_2)},
        {'z', std::bind(&Lexer::identifier, std::placeholders::_1, std::placeholders::_2)},
        {'A', std::bind(&Lexer::identifier, std::placeholders::_1, std::placeholders::_2)},
        {'B', std::bind(&Lexer::identifier, std::placeholders::_1, std::placeholders::_2)},
        {'C', std::bind(&Lexer::identifier, std::placeholders::_1, std::placeholders::_2)},
        {'D', std::bind(&Lexer::identifier, std::placeholders::_1, std::placeholders::_2)},
        {'E', std::bind(&Lexer::identifier, std::placeholders::_1, std::placeholders::_2)},
        {'F', std::bind(&Lexer::identifier, std::placeholders::_1, std::placeholders::_2)},
        {'G', std::bind(&Lexer::identifier, std::placeholders::_1, std::placeholders::_2)},
        {'H', std::bind(&Lexer::identifier, std::placeholders::_1, std::placeholders::_2)},
        {'I', std::bind(&Lexer::identifier, std::placeholders::_1, std::placeholders::_2)},
        {'J', std::bind(&Lexer::identifier, std::placeholders::_1, std::placeholders::_2)},
        {'K', std::bind(&Lexer::identifier, std::placeholders::_1, std::placeholders::_2)},
        {'L', std::bind(&Lexer::identifier, std::placeholders::_1, std::placeholders::_2)},
        {'M', std::bind(&Lexer::identifier, std::placeholders::_1, std::placeholders::_2)},
        {'N', std::bind(&Lexer::identifier, std::placeholders::_1, std::placeholders::_2)},
        {'O', std::bind(&Lexer::identifier, std::placeholders::_1, std::placeholders::_2)},
        {'P', std::bind(&Lexer::identifier, std::placeholders::_1, std::placeholders::_2)},
        {'Q', std::bind(&Lexer::identifier, std::placeholders::_1, std::placeholders::_2)},
        {'R', std::bind(&Lexer::identifier, std::placeholders::_1, std::placeholders::_2)},
        {'S', std::bind(&Lexer::identifier, std::placeholders::_1, std::placeholders::_2)},
        {'T', std::bind(&Lexer::identifier, std::placeholders::_1, std::placeholders::_2)},
        {'U', std::bind(&Lexer::identifier, std::placeholders::_1, std::placeholders::_2)},
        {'V', std::bind(&Lexer::identifier, std::placeholders::_1, std::placeholders::_2)},
        {'W', std::bind(&Lexer::identifier, std::placeholders::_1, std::placeholders::_2)},
        {'X', std::bind(&Lexer::identifier, std::placeholders::_1, std::placeholders::_2)},
        {'Y', std::bind(&Lexer::identifier, std::placeholders::_1, std::placeholders::_2)},
        {'Z', std::bind(&Lexer::identifier, std::placeholders::_1, std::placeholders::_2)},

        // character literal
        {'\'', std::bind(&Lexer::characterLiteral, std::placeholders::_1, std::placeholders::_2)},

        // number literal
        {'-', std::bind(&Lexer::numberLiteral, std::placeholders::_1, std::placeholders::_2)},
        {'0', std::bind(&Lexer::numberLiteral, std::placeholders::_1, std::placeholders::_2)},
        {'1', std::bind(&Lexer::numberLiteral, std::placeholders::_1, std::placeholders::_2)},
        {'2', std::bind(&Lexer::numberLiteral, std::placeholders::_1, std::placeholders::_2)},
        {'3', std::bind(&Lexer::numberLiteral, std::placeholders::_1, std::placeholders::_2)},
        {'4', std::bind(&Lexer::numberLiteral, std::placeholders::_1, std::placeholders::_2)},
        {'5', std::bind(&Lexer::numberLiteral, std::placeholders::_1, std::placeholders::_2)},
        {'6', std::bind(&Lexer::numberLiteral, std::placeholders::_1, std::placeholders::_2)},
        {'7', std::bind(&Lexer::numberLiteral, std::placeholders::_1, std::placeholders::_2)},
        {'8', std::bind(&Lexer::numberLiteral, std::placeholders::_1, std::placeholders::_2)},
        {'9', std::bind(&Lexer::numberLiteral, std::placeholders::_1, std::placeholders::_2)},

        // string literal
        {'"', std::bind(&Lexer::stringLiteral, std::placeholders::_1, std::placeholders::_2)},
    };

    bool Lexer::hasNext() const {
        auto position = _currentPosition;
        skipWhitespace(position);
        return position <= _code.length();
    }
    
    Token Lexer::next() {
        if (_currentPosition >= _code.length()) {
            _currentPosition++;
            return Token { { _sourceLocation, _sourceLocation }, TokenType::END_OF_FILE };
        }

        skipWhitespace(_currentPosition);
        updateLocation();

        auto ch = read(false);
        Token token {{ _source, _source }, TokenType::NONE, std::string(1, ch)};

        auto case_range = cases.equal_range(ch);
        bool hasFoundMatchingCase = false;
        for (auto it = case_range.first; it != case_range.second; ++it) {
            if (it->second(this, token)) {
                hasFoundMatchingCase = true;
                break;
            }
        }

        if (!hasFoundMatchingCase) {
            auto startLocation = _sourceLocation;
            auto c = read();
            updateLocation();
            auto endLocation = _sourceLocation;
            token.sourceRange = { startLocation, endLocation };
            token.type = TokenType::ERROR;
            token.value = std::string(1, c);
            throw LexerException(token, "Illegal char sequence encountered.");
        }

        return token;
    }

    /*
     * updateLocation should only be called, if we are committing to
     * the updatedLocation. There is no easy way to revert the location.
     */
    void Lexer::updateLocation() {
        for (auto i = _sourceLocation.charPos(); i < _currentPosition; i++) {
            const auto c = _code[i];
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
        while (isWhitespace(_code[position])) {
            position++;
        }
    }

    char Lexer::read(bool advancePosition) {
        if (_currentPosition == _code.length()) {
            return '\0';
        }
        auto character = _code[_currentPosition];
        if (advancePosition) {
            _currentPosition++;
        }
        return character;
    }

    bool Lexer::isWhitespace(char c) const {
        return c == ' ' || c == '\t' || c == '\n';
    }

    bool Lexer::isAlpha(char c) const {
        return isalpha(c);
    }

    bool Lexer::isAlphanumeric(char c) const {
        return isalnum(c);
    }

    bool Lexer::isDigit(char c) const {
        return isdigit(c);
    }

    bool Lexer::isHexDigit(char c) const {
        return isxdigit(c);
    }

    bool Lexer::readSingleLineToken(Token& token, TokenType type) {
        auto c = read();
        auto start = _sourceLocation;
        updateLocation();
        auto end = _sourceLocation;
        token.type = type;
        token.sourceRange = { start, end };
        token.value = std::string(1, c);
        return true;
    }

    bool Lexer::matches(const std::string& str, bool allowAlphanumericAtEnd) {
        size_t pos = 0;
        char c;
        while(pos < str.size()) {
            c = read();
            if (c != str[pos]) {
                return false;
            }
            pos++;
        }
        if (!allowAlphanumericAtEnd)
            return !isAlphanumeric(read(false));
        return true;
    }
    
    bool Lexer::matchesKeyword(Token& token, const std::string& keyword, TokenType type, bool allowAlphanumericAtEnd) {
        auto startLocation = _sourceLocation;
        auto keywordStart = _currentPosition;
        if (matches(keyword, allowAlphanumericAtEnd)) {
            auto keywordEnd = _currentPosition;
            updateLocation();
            auto endLocation = _sourceLocation;
            token.sourceRange = { startLocation, endLocation };
            token.type = type;
            token.value = _code.substr(keywordStart, keywordEnd - keywordStart);
            return true;
        }
        _currentPosition = keywordStart;
        return false;
    }

    char Lexer::getEscapedValue(char unescaped) {
        char escaped;
        switch(unescaped) {
            case '\'':
                escaped = 0x27;
                break;
            case '"':
                escaped = 0x22;
                break;
            case '?':
                escaped = 0x3f;
                break;
            case '\\':
                escaped = 0x5c;
                break;
            case 'a':
                escaped = 0x07;
                break;
            case 'b':
                escaped = 0x08;
                break;
            case 'f':
                escaped = 0x0c;
                break;
            case 'n':
                escaped = 0x0a;
                break;
            case 'r':
                escaped = 0x0d;
                break;
            case 't':
                escaped = 0x09;
                break;
            case 'v':
                escaped = 0x0b;
                break;
            default:
                escaped = -1;
        }
        return escaped;
    }

    bool Lexer::hexLiteral(Token& token, std::stringstream& content) {
        char c;
        uint8_t nibbles = 0;

        while(isHexDigit(c = read(false))) {
            content << c;
            _currentPosition++;
            nibbles++;
        }

        if (nibbles == 0 || nibbles > 16) {
            return false;
        }

        token.radix = 16;
        token.numberType = NumberType::UINT;
        return true;
    }

    bool Lexer::binaryLiteral(Token& token, std::stringstream& content) {
        char c;
        uint8_t bits = 0;
        
        while((c = read(false)) == '0' || c == '1') {
            content << c;
            _currentPosition++;
            bits++;
        }

        if (bits == 0 || bits > 64) {
            return false;
        }

        token.radix = 2;
        token.numberType = NumberType::UINT;
        return true;
    }

    bool Lexer::decimalLiteral(Token& token, std::stringstream& content) {
        char c;
        bool atleastOneDigitBeforeDot = false;
        while(isDigit(c = read(false))) {
            content << c;
            _currentPosition++;
            atleastOneDigitBeforeDot = true;
        }
        if (!atleastOneDigitBeforeDot) {
            return false;
        }
        if ((c = read(false)) == '.') {
            content << c;
            _currentPosition++;
            bool atleastOneDecimal = false;
            while(isDigit(c = read(false))) {
                content << c;
                _currentPosition++;
                atleastOneDecimal = true;
            }

            if (!atleastOneDecimal) {
                return false;
            }

            // TODO: allow 1.7976931348623157E+308 notation

            token.numberType = NumberType::FLOAT;
        } else {
            if (read(false) == 'u') {
                _currentPosition++;
                token.numberType = NumberType ::UINT;
            } else {
                token.numberType = NumberType::INT;
            }
        }
        token.radix = 10;
        return true;
    }


    bool Lexer::blockComment(Token& token) {
        auto commentStart = _currentPosition;
        auto startLocation = _sourceLocation;

        // ignore the /
        read();
        if (read() != '*') {
            _currentPosition = commentStart;
            return false;
        }
        
        while(_currentPosition < _code.length() - 1) {
            while(read() != '*') {
                if (_currentPosition == _code.length() - 1) {
                    _currentPosition = commentStart;
                    return false;
                }
            }

            if (read() == '/') {
                auto commentEnd = _currentPosition;
                updateLocation();
                auto endLocation = _sourceLocation;
                token.type = TokenType::BLOCK_COMMENT;
                token.sourceRange = { startLocation, endLocation };
                token.value = _code.substr(commentStart, commentEnd - commentStart);
                return true;
            }
        }

        _currentPosition = commentStart;
        return false;
    }

    bool Lexer::lineComment(Token& token) {
        auto commentStart = _currentPosition;
        auto startLocation = _sourceLocation;

        // ignore first /
        read();
        if (read() != '/') {
            _currentPosition = commentStart;
            return false;
        }

        // read while we have not reached the end of the line or the end of the file
        while(read(false) != '\n' && _currentPosition < _code.length()) {
            _currentPosition++;
        }

        auto commentEnd = _currentPosition;
        updateLocation();
        auto endLocation = _sourceLocation;
        token.type = TokenType::LINE_COMMENT;
        token.sourceRange = { startLocation, endLocation };
        token.value = _code.substr(commentStart, commentEnd - commentStart);
        return true;
    }

    bool Lexer::pubKeyword(Token& token) {
        return matchesKeyword(token, "pub", TokenType::PUB);
    }

    bool Lexer::externKeyword(Token& token) {
        return matchesKeyword(token, "extern", TokenType::EXTERN);
    }

    bool Lexer::funKeyword(Token& token) {
        return matchesKeyword(token, "fun", TokenType::FUN);
    }

    bool Lexer::sizeOfKeyword(Token& token) {
        return matchesKeyword(token, "size_of", TokenType::SIZE_OF, true);
    }

    bool Lexer::returnKeyword(Token& token) {
        return matchesKeyword(token, "return", TokenType::RETURN);
    }

    bool Lexer::ifKeyword(Token& token) {
        return matchesKeyword(token, "if", TokenType::IF);
    }

    bool Lexer::elseKeyword(Token& token) {
        return matchesKeyword(token, "else", TokenType::ELSE);
    }

    bool Lexer::whileKeyword(Token& token) {
        return matchesKeyword(token, "while", TokenType::WHILE);
    }

    bool Lexer::forKeyword(Token& token) {
        return matchesKeyword(token, "for", TokenType::FOR);
    }

    bool Lexer::doKeyword(Token& token) {
        return matchesKeyword(token, "do", TokenType::DO);
    }
    
    bool Lexer::letKeyword(Token& token) {
        return matchesKeyword(token, "let", TokenType::LET);
    }

    bool Lexer::constKeyword(Token& token) {
        return matchesKeyword(token, "const", TokenType::CONST);
    }

    bool Lexer::arrow(Token& token) {
        auto arrowStart = _currentPosition;
        auto startLocation = _sourceLocation;

        // ignore first -
        read();
        if (read() != '>') {
            _currentPosition = arrowStart;
            return false;
        }

        auto arrowEnd = _currentPosition;
        updateLocation();
        auto endLocation = _sourceLocation;
        token.type = TokenType::ARROW;
        token.sourceRange = { startLocation, endLocation };
        token.value = _code.substr(arrowStart, arrowEnd - arrowStart);
        return true;
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

    bool Lexer::orOp(Token& token) {
		auto orStart = _currentPosition;
		auto startLocation = _sourceLocation;
		// ignore first |
		read();
		if (read() != '|') {
			_currentPosition = orStart;
			return false;
		}

		auto orEnd = _currentPosition;
		updateLocation();
		auto endLocation = _sourceLocation;
		token.type = TokenType::OR;
		token.sourceRange = { startLocation, endLocation };
		token.value = _code.substr(orStart, orEnd - orStart);
		return true;
    }

    bool Lexer::lslOp(Token& token) {
        return matchesKeyword(token, "lsl", TokenType::LSL);
    }

    bool Lexer::lsrOp(Token& token) {
        return matchesKeyword(token, "lsr", TokenType::LSR);
    }

    bool Lexer::asrOp(Token& token) {
        return matchesKeyword(token, "asr", TokenType::ASR);
    }

    bool Lexer::minus(Token& token) {
        return readSingleLineToken(token, TokenType::MINUS);
    }

    bool Lexer::hash(Token& token) {
        return readSingleLineToken(token, TokenType::HASH);
    }

    bool Lexer::atSign(Token& token) {
        return readSingleLineToken(token, TokenType::AT_SIGN);
    }

    bool Lexer::dollar(Token& token) {
        return readSingleLineToken(token, TokenType::DOLLAR);
    }

    bool Lexer::backslash(Token& token) {
        return readSingleLineToken(token, TokenType::BACKSLASH);
    }

    bool Lexer::backquote(Token& token) {
        return readSingleLineToken(token, TokenType::BACKQUOTE);
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

    bool Lexer::semicolon(Token& token) {
        return readSingleLineToken(token, TokenType::SEMICOLON);
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

    bool Lexer::andOp(Token& token) {
		auto andStart = _currentPosition;
		auto startLocation = _sourceLocation;
		// ignore first &
		read();
		if (read() != '&') {
			_currentPosition = andStart;
			return false;
		}

		auto andEnd = _currentPosition;
		updateLocation();
		auto endLocation = _sourceLocation;
		token.type = TokenType::AND;
		token.sourceRange = { startLocation, endLocation };
		token.value = _code.substr(andStart, andEnd - andStart);
		return true;
    }

    bool Lexer::assignOp(Token& token) {
        return readSingleLineToken(token, TokenType::ASSIGN_OP);
    }

    bool Lexer::equal(Token& token) {
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
        token.sourceRange = { startLocation, endLocation };
        token.value = _code.substr(equalStart, equalEnd - equalStart);
        return true;
    }

    bool Lexer::notEqual(Token& token) {
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
        token.sourceRange = { startLocation, endLocation };
        token.value = _code.substr(equalStart, equalEnd - equalStart);
        return true;
    }

    bool Lexer::lessThan(Token& token) {
        return readSingleLineToken(token, TokenType::LT_OP);
    }

    bool Lexer::greaterThan(Token& token) {
        return readSingleLineToken(token, TokenType::GT_OP);
    }

    bool Lexer::lessThanEqual(Token& token) {
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
        token.sourceRange = { startLocation, endLocation };
        token.value = _code.substr(lessThanEqualStart, lessThanEqualEnd - lessThanEqualStart);
        return true;
    }

    bool Lexer::greaterThanEqual(Token& token) {
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
        token.sourceRange = { startLocation, endLocation };
        token.value = _code.substr(greaterThanEqualStart, greaterThanEqualEnd - greaterThanEqualStart);
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

    bool Lexer::leftSquaredBracket(Token& token) {
        return readSingleLineToken(token, TokenType::LEFT_SQUARED_BRACKET);
    }

    bool Lexer::rightSquaredBracket(Token& token) {
        return readSingleLineToken(token, TokenType::RIGHT_SQUARED_BRACKET);
    }

    bool Lexer::identifier(Token& token) {
        auto identifierStart = _currentPosition;
        auto startLocation = _sourceLocation;
        char c = read();
        if (!isAlpha(c)) {
            _currentPosition = identifierStart;
            return false;
        }
        while((c = read(false)) && isAlphanumeric(c)) {
            // skip alphanumeric chars
            _currentPosition++;
        }

        auto identifierEnd = _currentPosition;
        updateLocation();
        auto endLocation = _sourceLocation;
        token.type = TokenType::IDENTIFIER;
        token.sourceRange = { startLocation, endLocation };
        token.value = _code.substr(identifierStart, identifierEnd - identifierStart);
        return true;
    }

    bool Lexer::characterLiteral(Token& token) {
        auto code = _source->code();
        auto characterLiteralStart = _currentPosition;
        auto startLocation = _sourceLocation;
        char content;
        // first ' char
        char c = read();
        // escaped char
        if ((c = read()) == '\\') {
            c = read();
            content = getEscapedValue(c);
            // escape error
            if (content == -1) {
                _currentPosition = characterLiteralStart;
                return false;
            }
        } else if (c == '\'') {
            // if there is no char, we bail
            _currentPosition = characterLiteralStart;
            return false;
        } else {
            content = c;
        }
        if ((c = read()) == '\'') {
            updateLocation();
            auto endLocation = _sourceLocation;
            token.type = TokenType::CHARACTER_LITERAL;
            token.sourceRange = { startLocation, endLocation };
            token.value = content;
            return true;
        }
        _currentPosition = characterLiteralStart;
        return false;
    }

    bool Lexer::numberLiteral(Token& token) {
        auto numberLiteralStart = _currentPosition;
        auto startLocation = _sourceLocation;
        std::stringstream content;
        uint8_t radix = 10;
        
        char c = read(false);

        // parse radix
        if (c == '0') {
            // skip the '0'
            read();
            switch(read(false)) {
            case 'x':
            case 'X':
                radix = 16;
                read();
                break;
            case 'b':
            case 'B':
                radix = 2;
                read();
                break;
            default:
                radix = 10;
                _currentPosition--;
                break;
            }
        }

        bool result;

        switch(radix) {
        case 16:
            result = hexLiteral(token, content);
            break;
        case 2:
            result = binaryLiteral(token, content);
            break;
        case 10:
        default:
            result = decimalLiteral(token, content);
            break;
        }

        if (result) {
            updateLocation();
            auto endLocation = _sourceLocation;

            token.type = TokenType::NUMBER_LITERAL;
            token.sourceRange = { startLocation, endLocation };
            token.value = content.str();
            return true;
        } else {
            _currentPosition = numberLiteralStart;
            return false;
        }
    }

    bool Lexer::stringLiteral(Token& token) {
        auto stringLiteralStart = _currentPosition;
        auto startLocation = _sourceLocation;
        std::stringstream content;

        // skip first '"'
        char c = read();
        while((c = read(false)) != '"') {
            if (_currentPosition == _code.length()) {
                _currentPosition = stringLiteralStart;
                return false;
            }
            // escaped char
            if (read(false) == '\\') {
                _currentPosition++;
                // TODO: refactor this
                if (_currentPosition == _code.length()) {
                    _currentPosition = stringLiteralStart;
                    return false;
                }
                char unescaped = read(false);
                char escaped = getEscapedValue(unescaped);
                // escape error
                if (escaped == -1) {
                    _currentPosition = stringLiteralStart;
                    return false;
                }
                content << escaped;
            } else {
                content << c;
            }
            _currentPosition++;
        }
        // skip the terminating '"'
        _currentPosition++;

        updateLocation();
        auto endLocation = _sourceLocation;
        token.type = TokenType::STRING_LITERAL;
        token.sourceRange = { startLocation, endLocation };
        token.value = content.str();
        return true;
    }

    bool Lexer::trueKeyword(Token& token) {
        return matchesKeyword(token, "true", TokenType::TRUE_KEYWORD);
    }

    bool Lexer::falseKeyword(Token& token) {
        return matchesKeyword(token, "false", TokenType::FALSE_KEYWORD);
    }

    bool Lexer::ptrType(klong::Token &token) {
        return matchesKeyword(token, "ptr", TokenType::PTR);
    }

    bool Lexer::voidType(Token& token) {
        return matchesKeyword(token, "void", TokenType::VOID);
    }

    bool Lexer::stringType(Token& token) {
        return matchesKeyword(token, "string", TokenType::STRING);
    }

    bool Lexer::boolType(Token& token) {
        return matchesKeyword(token, "bool", TokenType::BOOL);
    }

    bool Lexer::i8Type(Token& token) {
        return matchesKeyword(token, "i8", TokenType::I8);
    }

    bool Lexer::i16Type(Token& token) {
        return matchesKeyword(token, "i16", TokenType::I16);
    }

    bool Lexer::i32Type(Token& token) {
        return matchesKeyword(token, "i32", TokenType::I32);
    }

    bool Lexer::i64Type(Token& token) {
        return matchesKeyword(token, "i64", TokenType::I64);
    }

    bool Lexer::u8Type(Token& token) {
        return matchesKeyword(token, "u8", TokenType::U8);
    }

    bool Lexer::u16Type(Token& token) {
        return matchesKeyword(token, "u16", TokenType::U16);
    }

    bool Lexer::u32Type(Token& token) {
        return matchesKeyword(token, "u32", TokenType::U32);
    }

    bool Lexer::u64Type(Token& token) {
        return matchesKeyword(token, "u64", TokenType::U64);
    }

    bool Lexer::f32Type(Token& token) {
        return matchesKeyword(token, "f32", TokenType::F32);
    }

    bool Lexer::f64Type(Token& token) {
        return matchesKeyword(token, "f64", TokenType::F64);
    }

}