#pragma once

#include <map>
#include <sstream>
#include <functional>

#include "lexer/lexer_memento.h"
#include "lexer/token.h"
#include "common/source_file.h"

namespace klong {
    class Lexer {
    public:
        using LexerCaseCallable = std::function<bool (Lexer*, Token&)>;

        explicit Lexer(std::shared_ptr<SourceFile> source) : _source(source), _code(source->code()) {
            _sourceLocation = SourceLocation(source);
        }

        Token next();

        LexerMemento saveToMemento();
        void loadFromMemento(LexerMemento& memento);

        std::string filename() const {
            return _source->filename();
        }

        std::string absolutepath() const {
            return _source->absolutepath();
        }

    private:
        void updateLocation();

        inline void skipWhitespace() {
            while (isWhitespace(_code[_currentPosition])) {
                _currentPosition++;
            }
        }

        inline char peek() {
            if (_currentPosition == _code.length()) {
                return '\0';
            }
            return _code[_currentPosition];
        }

        inline char advance() {
            char character = peek();
            _currentPosition++;
            return character;
        }

        inline bool isWhitespace(char c) const {
            return c == ' ' || c == '\t' || c == '\n';
        }

        inline bool isAlpha(char c) const {
            return isalpha(c);
        }

        inline bool isAlphanumeric(char c) const {
            return isalnum(c);
        }

        inline bool isDigit(char c) const {
            return isdigit(c);
        }

        inline bool isHexDigit(char c) const {
            return isxdigit(c);
        }

        bool readSingleCharacterToken(Token& token, TokenType type);
        bool matches(const std::string& str);
        bool matchesKeyword(Token& token, const std::string& keyword, TokenType type);
        char getEscapedValue(char valueToEscape);

        bool hexLiteral(Token& token, std::stringstream& content);
        bool binaryLiteral(Token& token, std::stringstream& content);
        bool decimalLiteral(Token& token, std::stringstream& content);

        bool importKeyword(Token& token);

        bool blockComment(Token& token);
        bool lineComment(Token& token);

        bool pubKeyword(Token& token);
        bool externKeyword(Token& token);
		bool structKeyword(Token& token);
		bool unionKeyword(Token& token);
		bool enumKeyword(Token& token);

        bool funKeyword(Token& token);
        bool sizeofKeyword(Token& token);
        bool castKeyword(Token& token);
        bool returnKeyword(Token& token);

        bool ifKeyword(Token& token);
        bool elseKeyword(Token& token);
        bool whileKeyword(Token& token);
        bool forKeyword(Token& token);
        bool doKeyword(Token& token);
        bool breakKeyword(Token& token);
        bool continueKeyword(Token& token);
        bool deferKeyword(Token& token);

        bool letKeyword(Token& token);
        bool constKeyword(Token& token);

        bool nullKeyword(Token& token);

        bool arrow(Token& token);
        bool plus(Token& token);
        bool bang(Token& token);
        bool minus(Token& token);
        bool hash(Token& token);
        bool atSign(Token& token);
        bool dollar(Token& token);
        bool backslash(Token& token);
        bool backquote(Token& token);
        bool pipe(Token& token);
        bool orOp(Token& token);
        bool lslOp(Token& token);
        bool lsrOp(Token& token);
        bool asrOp(Token& token);
        bool slash(Token& token);
        bool caret(Token& token);
        bool tilde(Token& token);
        bool semicolon(Token& token);
        bool colon(Token& token);
        bool comma(Token& token);
        bool spread(Token& token);
        bool period(Token& token);
        bool percent(Token& token);
        bool question(Token& token);
        bool asterisk(Token& token);
        bool ampersand(Token& token);
        bool andOp(Token& token);
        bool assignOp(Token& token);
        bool equal(Token& token);
        bool notEqual(Token& token);
        bool lessThan(Token& token);
        bool greaterThan(Token& token);
        bool lessThanEqual(Token& token);
        bool greaterThanEqual(Token& token);
        bool leftCurlyBrace(Token& token);
        bool rightCurlyBrace(Token& token);
        bool leftParenthesis(Token& token);
        bool rightParenthesis(Token& token);
        bool leftSquaredBracket(Token& token);
        bool rightSquaredBracket(Token& token);

        bool identifier(Token& token);

        bool characterLiteral(Token& token);
        bool numberLiteral(Token& token);
        bool stringLiteral(Token& token);
        bool trueKeyword(Token& token);
        bool falseKeyword(Token& token);

        bool ptrType(Token& token);
        bool voidType(Token& token);
        bool boolType(Token& token);
        bool i8Type(Token& token);
        bool i16Type(Token& token);
        bool i32Type(Token& token);
        bool i64Type(Token& token);
        bool u8Type(Token& token);
        bool u16Type(Token& token);
        bool u32Type(Token& token);
        bool u64Type(Token& token);
        bool f32Type(Token& token);
        bool f64Type(Token& token);

    private:
        static std::multimap<char, LexerCaseCallable> cases;

        std::shared_ptr<SourceFile> _source;
        std::string _code;
        SourceLocation _sourceLocation;
        size_t _currentPosition = 0;
    };
}