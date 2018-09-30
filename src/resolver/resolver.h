#pragma once

#include "resolver/resolve_visitor.h"
#include "ast/module.h"

namespace klong {
    class Resolver {
    public:
        void resolve(Module& module) {
            auto resolveVisitor = ResolveVisitor();
            module.accept(&resolveVisitor);
        }
    };
}