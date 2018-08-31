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
                _charPos++;
            }

            inline void incCol() {
                _column++;
                _charPos++;
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

            inline size_t charPos() const {
                return _charPos;
            }

        private:
            SourceFile* _source;
            size_t _line = 1;
            size_t _column = 1;
            size_t _charPos = 0;
    };
}