#pragma once

#include "array.h"

#include "ast/module.h"
#include "compilation_incident.h"

namespace klong {
    class CompilationResult {
    public:
        CompilationResult() = default;

        ModulePtr success() const;
        void setSuccess(ModulePtr module);

        bool hasErrors() const;
        bool hasAnyReportableIncidents() const;

        void addIncident(CompilationIncident&& incident);

        Array<CompilationIncident> getIncidents() const;
    private:
        bool _hasErrors = false;
        ModulePtr _module;
        Array<CompilationIncident> _incidents;
    };
}