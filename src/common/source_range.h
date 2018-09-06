#pragma once

#include "source_location.h"

namespace klong {
    struct SourceRange {
        SourceRange(SourceLocation startLocation, SourceLocation endLocation):
            start(startLocation), end(endLocation) {
        }

        SourceRange() : start(), end() {

        }

        bool valid() const {
            return start.valid() && end.valid();
        }

        SourceLocation start;
        SourceLocation end;
    };
}