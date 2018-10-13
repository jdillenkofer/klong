#include "typechecker.h"

namespace klong {
    Result<ModulePtr, TypeCheckException> TypeChecker::check(ModulePtr module) {
        auto typeCheckVisitor = TypeCheckVisitor();
        module->accept(&typeCheckVisitor);
        return typeCheckVisitor.getResult();
    }
}