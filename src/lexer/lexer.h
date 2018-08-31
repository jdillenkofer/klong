#pragma once

#include "token.h"
#include "../common/source_file.h"

namespace klong {
    class Lexer {
        public:
            Lexer(SourceFile source) : _source(source), _sourceLocation(&source) {
            }

            bool hasNext() const;
            Token next();
            Token peek() const;

        private:
            void updateLocation(size_t position, SourceLocation& location) const;
            void skipWhitespace(size_t& position) const;
            
            inline bool isWhitespace(char c) const {
                return c == ' ' || c == '\t' || c == '\n';
            }

            Token determineNextToken(size_t& position, SourceLocation& location) const;

        private:
            SourceFile _source;
            SourceLocation _sourceLocation;
            size_t _currentPosition = 0;
    };
}