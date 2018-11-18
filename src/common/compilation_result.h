#pragma once

#include <vector>

#include "ast/module.h"
#include "compilation_error.h"
#include "compilation_warning.h"

namespace klong {
    class CompilationResult {
    public:
        CompilationResult() = default;

        ModulePtr success() const;
        void setSuccess(ModulePtr module);

        bool hasErrors() const;
        bool hasWarnings() const;

        void addError(CompilationError&& error);
        void addWarning(CompilationWarning&& warning);

        std::vector<CompilationError> getErrors() const;
        std::vector<CompilationWarning> getWarnings() const;
    private:
        ModulePtr _module;
        std::vector<CompilationError> _errors;
        std::vector<CompilationWarning> _warnings;
    };
}