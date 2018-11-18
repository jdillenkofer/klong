#pragma once

#include "source_range.h"

namespace klong {
    class CompilationError : public std::exception {
    public:
        CompilationError(SourceRange sourceRange, std::string message):
                _sourceRange(sourceRange), _message(std::move(message)) {
        }

        const char* what() const noexcept override {
            return _message.c_str();
        }

        SourceRange sourceRange() const {
            return _sourceRange;
        }

    private:
        SourceRange _sourceRange;
        std::string _message;
    };
}