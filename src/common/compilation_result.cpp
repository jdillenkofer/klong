#include "compilation_result.h"

namespace klong {
    ModulePtr CompilationResult::success() const {
        return _module;
    }

    void CompilationResult::setSuccess(ModulePtr module) {
        _module = std::move(module);
    }

    bool CompilationResult::hasErrors() const {
        return !_errors.empty();
    }

    bool CompilationResult::hasWarnings() const {
        return !_warnings.empty();
    }

    void CompilationResult::addError(CompilationError&& error) {
        _errors.emplace_back(error);
    }

    void CompilationResult::addWarning(CompilationWarning&& warning) {
        _warnings.emplace_back(warning);
    }

    std::vector<CompilationError> CompilationResult::getErrors() const {
        return _errors;
    }

    std::vector<CompilationWarning> CompilationResult::getWarnings() const {
        return _warnings;
    }
}