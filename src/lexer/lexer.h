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
            
            inline bool isWhitespace(char c) const {
                return c == ' ' || c == '\t' || c == '\n';
            }

            inline bool readSingleLineToken(Token& token, TokenType type) {
                auto c = read();
                auto start = _sourceLocation;
                updateLocation();
                auto end = _sourceLocation;
                token.type = type;
                token.start = start;
                token.end = end;
                token.value = std::string(1, c);
                return true;
            }

            inline bool matches(const std::string& str) {
                size_t pos = 0;
                char c = read();
                while(pos < (str.size() - 1)) {
                    if (c != str[pos]) {
                        return false;
                    }
                    c = read();
                    pos++;
                }
                // expect whitespace behind the match
                if (isWhitespace(read(false))){
                    return true;
                }
                return false;
            }

            inline bool matchesKeyword(Token& token, const std::string& keyword, TokenType type) {
                auto code = _source.code();
                auto startLocation = _sourceLocation;
                auto keywordStart = _currentPosition;
                if (matches(keyword)) {
                    auto keywordEnd = _currentPosition;
                    updateLocation();
                    auto endLocation = _sourceLocation;
                    token.start = startLocation;
                    token.end = endLocation;
                    token.type = type;
                    token.value = code.substr(keywordStart, keywordEnd - keywordStart);
                    return true;
                }

                _currentPosition = keywordStart;
                return false;
            }

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
            bool blockComment(Token& token);
            bool lineComment(Token& token);

        private:
            static std::multimap<char, LexerCaseCallable> cases;
            
            SourceFile _source;
            SourceLocation _sourceLocation;
            size_t _currentPosition = 0;

    };
}