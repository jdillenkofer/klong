#pragma once

#include <cstdlib>
#include "source_file.h"

namespace klong {
    class SourceLocation {
        public:
            SourceLocation(SourceFile* source): _source(source) {
            }

            inline void incLine() {
                _line++;
                _column = 1;
            }

            inline void incCol() {
                _column++;
            }

            inline const std::string& filepath() const {
                return _source->path();
            }

            inline size_t column() const {
                return _column;
            }

            inline size_t line() const {
                return _line;
            }

        private:
            SourceFile* _source;
            size_t _line = 1;
            size_t _column = 1;
    };
}