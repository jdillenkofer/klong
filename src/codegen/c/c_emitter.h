#pragma once

#include "common/compilation_session.h"
#include "ast/module.h"
#include "c_emit_visitor.h"

namespace klong {
    class CEmitter {
    public:
        CEmitter() {
            _cEmitVisitor = std::make_unique<CEmitVisitor>();
        }

        void emit(ModulePtr module, CompilationSession* session);
        bool writeToFile(const std::string& filename);
    private:
        std::unique_ptr<CEmitVisitor> _cEmitVisitor;
    };
}