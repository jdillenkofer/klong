#pragma once

#include "type_check_visitor.h"
#include "ast/module.h"

namespace klong {
    class TypeChecker {
    public:
        void check(Module& module) {
            auto typeCheckVisitor = TypeCheckVisitor();
            module.accept(&typeCheckVisitor);
        }
    };
}