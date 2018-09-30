#pragma once

#include <cstdlib>
#include "source_file.h"

namespace klong {
    class SourceLocation {
    public:
        SourceLocation(): _source(nullptr), _valid(false) {
        }
            
        SourceLocation(SourceFile* source): _source(source), _valid(true) {
        }

        void incLine() {
            _line++;
            _column = 1;
            _charPos++;
        }

        void incCol() {
            _column++;
            _charPos++;
        }

        std::string filepath() const {
            return _source->path();
        }

        size_t column() const {
            return _column;
        }

        size_t line() const {
            return _line;
        }

        size_t charPos() const {
            return _charPos;
        }

        bool valid() const {
            return _valid;
        }

    private:
        SourceFile* _source;
        size_t _line = 1;
        size_t _column = 1;
        size_t _charPos = 0;
        bool _valid;
    };
}