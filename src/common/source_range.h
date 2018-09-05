#pragma once

#include "source_location.h"

namespace klong {
    struct SourceRange {
        SourceRange(SourceLocation startLocation, SourceLocation endLocation) {
            start = startLocation;
            end = endLocation;
        }
        SourceLocation start;
        SourceLocation end;
    };
}