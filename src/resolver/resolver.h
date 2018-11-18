#pragma once

#include "common/compilation_result.h"
#include "resolver/resolve_visitor.h"
#include "ast/module.h"

namespace klong {
    class Resolver {
    public:
        void resolve(ModulePtr module, CompilationResult* result);
    };
}