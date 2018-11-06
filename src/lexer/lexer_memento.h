#pragma once

#include <string>
#include "common/source_location.h"

namespace klong {
    class Lexer;

    class LexerMemento {
    public:
        LexerMemento(SourceFile* sourceFile, std::string code, SourceLocation sourceLocation, size_t currentPosition):
            _sourceFile(sourceFile), _code(std::move(code)), _sourceLocation(sourceLocation), _currentPosition(currentPosition) {

        }
    private:
        SourceFile* _sourceFile;
        std::string _code;
        SourceLocation _sourceLocation;
        size_t _currentPosition;

        friend Lexer;
    };
}