#pragma once

#include "common/compilation_result.h"
#include "type_check_visitor.h"
#include "ast/module.h"

namespace klong {
    class TypeChecker {
    public:
        void check(ModulePtr module, CompilationResult* result);
    };
}