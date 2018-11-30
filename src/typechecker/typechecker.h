#pragma once

#include "common/compilation_session.h"
#include "type_check_visitor.h"
#include "ast/module.h"

namespace klong {
    class TypeChecker {
    public:
        void check(ModulePtr module, CompilationSession* session);
    };
}