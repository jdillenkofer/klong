#pragma once

#include "source_range.h"

namespace klong {
    enum class CompilationIncidentType {
        INFO = 0,
        WARNING,
        ERROR
    };

    struct CompilationIncident {
        CompilationIncident(CompilationIncidentType type, std::string message, SourceRange sourceRange):
            type(type), message(std::move(message)), sourceRange(sourceRange) {
        }

        inline const char* getTypeName() {
            switch (type) {
            case CompilationIncidentType::INFO:
                return "INFO";
            case CompilationIncidentType::WARNING:
                return "WARNING";
            case CompilationIncidentType::ERROR:
                return "ERROR";
            default:
                return "UNKNOWN";
            }
        }

        CompilationIncidentType type;
        std::string message;
        SourceRange sourceRange;
    };
}