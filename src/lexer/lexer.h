#pragma once

#include "token.h"
#include "../common/source_file.h"

namespace klong {
    class Lexer {
        public:
        Lexer(SourceFile source) : _source(source), _currentLocation(&source) {
        }

        bool hasNext() const;
        Token next();
        Token peek() const;
        void reset();

        private:
        SourceFile _source;
        SourceLocation _currentLocation;
    };
}