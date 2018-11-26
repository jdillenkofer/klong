#pragma once

#include <cstdlib>
#include "source_file.h"

namespace klong {
    class SourceLocation {
    public:
        SourceLocation(): _source(nullptr), _valid(false) {
        }
            
        explicit SourceLocation(std::shared_ptr<SourceFile> source):
            _source(std::move(source)),
            _valid(true) {
        }

        void incLine();
        void incCol();

        std::string absolutepath() const;
        std::string filename() const;
        std::string code() const;
        uint64_t column() const;
        uint64_t line() const;
        uint64_t charPos() const;
        bool valid() const;

    private:
        std::shared_ptr<SourceFile> _source;
        uint64_t _line = 1;
        uint64_t _column = 1;
        uint64_t _charPos = 0;
        bool _valid;
    };
}