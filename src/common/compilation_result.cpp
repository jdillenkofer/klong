#include "compilation_result.h"

namespace klong {
    ModulePtr CompilationResult::success() const {
        return _module;
    }

    void CompilationResult::setSuccess(ModulePtr module) {
        _module = std::move(module);
    }

    bool CompilationResult::hasErrors() const {
        return _hasErrors;
    }

    bool CompilationResult::hasAnyReportableIncidents() const {
        return _incidents.size() > 0;
    }

    void CompilationResult::addIncident(CompilationIncident&& incident) {
        if (incident.type == CompilationIncidentType::ERROR) {
            _hasErrors = true;
        }
        _incidents.push(incident);
    }

    Array<CompilationIncident> CompilationResult::getIncidents() const {
        return _incidents;
    }
}