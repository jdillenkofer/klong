#pragma once

#include "common/result.h"
#include "resolver/resolve_visitor.h"
#include "ast/module.h"

namespace klong {
    class Resolver {
    public:
        Result<ModulePtr, ResolveException> resolve(ModulePtr module) {
            auto resolveVisitor = ResolveVisitor();
            module->accept(&resolveVisitor);
            return resolveVisitor.getResult();
        }
    };
}