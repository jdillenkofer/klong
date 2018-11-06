#pragma once

#include "lexer/token.h"
#include "lexer/lexer_memento.h"
#include "iparser.h"

namespace klong {
    class Parser;

    class ParserMemento {
    public:
        ParserMemento(Token current,
                Token previous,
                LexerMemento&& lexerMemento,
                bool isInsideFunction,
                bool isInsideLoop,
                std::vector<ParseException> errors):
                    _current(std::move(current)), _previous(std::move(previous)), _lexerMemento(std::move(lexerMemento)),
                    _isInsideFunction(isInsideFunction), _isInsideLoop(isInsideLoop), _errors(std::move(errors)) {
        }
    private:
        Token _current;
        Token _previous;
        LexerMemento _lexerMemento;
        bool _isInsideFunction = false;
        bool _isInsideLoop = false;
        std::vector<ParseException> _errors;

        friend Parser;
    };
}