#pragma once

#include "source_location.h"

namespace klong {
    class SourceRange {
    public:
        SourceRange(SourceLocation startLocation, SourceLocation endLocation):
            start(startLocation), end(endLocation) {
        }

        SourceRange() : start(), end() {

        }

        bool valid() const;
        std::string getRelevantSourceText() const;

    private:
        uint64_t getStartIndexOfLine(uint64_t currentIndex) const;

    public:
        SourceLocation start;
        SourceLocation end;
    };
}