#pragma once

#include "common/compilation_session.h"
#include "resolver/resolve_visitor.h"
#include "ast/module.h"

namespace klong {
    class Resolver {
    public:
        void resolve(ModulePtr module, CompilationSession* result);
    };
}