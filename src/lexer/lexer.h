#pragma once

#include <map>
#include <functional>

#include "token.h"
#include "../common/source_file.h"

namespace klong {
    class Lexer {
        public:
            using LexerCaseCallable = std::function<bool (Lexer*, Token&)>;

            Lexer(SourceFile source) : _source(source), _sourceLocation(&source) {
            }

            bool hasNext() const;
            Token next();

        private:
            void updateLocation();
            void skipWhitespace(size_t& position) const;
            char read(bool advancePosition = true);
            bool isWhitespace(char c) const;
            bool readSingleLineToken(Token& token, TokenType type);
            bool matches(const std::string& str);
            bool matchesKeyword(Token& token, const std::string& keyword, TokenType type);

            bool blockComment(Token& token);
            bool lineComment(Token& token);

            bool ifKeyword(Token& token);
            bool elseKeyword(Token& token);
            bool whileKeyword(Token& token);
            bool forKeyword(Token& token);
            bool doKeyword(Token& token);

            bool letKeyword(Token& token);
            bool constKeyword(Token& token);

            bool plus(Token& token);
            bool bang(Token& token);
            bool pipe(Token& token);
            bool minus(Token& token);
            bool slash(Token& token);
            bool caret(Token& token);
            bool tilde(Token& token);
            bool colon(Token& token);
            bool comma(Token& token);
            bool period(Token& token);
            bool percent(Token& token);
            bool question(Token& token);
            bool asterisk(Token& token);
            bool ampersand(Token& token);
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

        private:
            static std::multimap<char, LexerCaseCallable> cases;
            
            SourceFile _source;
            SourceLocation _sourceLocation;
            size_t _currentPosition = 0;

    };
}