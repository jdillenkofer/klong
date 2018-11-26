#pragma once

#include <string>
#include "common/source_location.h"

namespace klong {
    class Lexer;

    class LexerMemento {
    public:
        LexerMemento(std::shared_ptr<SourceFile> sourceFile, std::string code, SourceLocation sourceLocation, size_t currentPosition):
            _sourceFile(std::move(sourceFile)),
            _code(std::move(code)),
            _sourceLocation(std::move(sourceLocation)),
            _currentPosition(currentPosition) {

        }
    private:
        std::shared_ptr<SourceFile> _sourceFile;
        std::string _code;
        SourceLocation _sourceLocation;
        size_t _currentPosition;

        friend Lexer;
    };
}