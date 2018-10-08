#pragma once

#include "common/result.h"
#include "type_check_visitor.h"
#include "ast/module.h"

namespace klong {
    class TypeChecker {
    public:
        Result<ModulePtr, TypeCheckException> check(ModulePtr module) {
            auto typeCheckVisitor = TypeCheckVisitor();
            module->accept(&typeCheckVisitor);
            return typeCheckVisitor.getResult();
        }
    };
}