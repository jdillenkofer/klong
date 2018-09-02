#pragma once

#include <string>
#include <memory>

#include "iparser.h"
#include "../lexer/ilexer.h"
#include "expr.h"

#include "visitor.h" // <-- TODO: remove this include

namespace klong {
    class Parser {
        public:
            Parser(ILexer* lexer):
                _lexer(lexer) {
                advance();
            }

            std::vector<StmtPtr> parse();
        private:
            Token consume(TokenType type, std::string errorMessage);
            bool check(TokenType type);
            Token peek();
            Token previous();
            Token advance();
            bool isAtEnd();
        private:
            Token _current;
            Token _previous;
            ILexer* _lexer;
    };
}