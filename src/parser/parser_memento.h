#pragma once

#include "common/compilation_result.h"
#include "lexer/token.h"
#include "lexer/lexer_memento.h"

namespace klong {
    class Parser;

    class ParserMemento {
    public:
        ParserMemento(Token current,
                Token previous,
                LexerMemento&& lexerMemento,
                bool isInsideFunction,
                bool isInsideLoop,
                bool isInsideDefer,
                CompilationResult compilationResult):
                    _current(std::move(current)), _previous(std::move(previous)), _lexerMemento(std::move(lexerMemento)),
                    _isInsideFunction(isInsideFunction), _isInsideLoop(isInsideLoop), _isInsideDefer(isInsideDefer), _compilationResult(std::move(compilationResult)) {
        }
    private:
        Token _current;
        Token _previous;
        LexerMemento _lexerMemento;
        bool _isInsideFunction = false;
        bool _isInsideLoop = false;
        bool _isInsideDefer = false;
        CompilationResult _compilationResult;

        friend Parser;
    };
}