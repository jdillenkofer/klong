#include "typechecker.h"

namespace klong {
    void TypeChecker::check(ModulePtr module, CompilationResult* result) {
        auto typeCheckVisitor = TypeCheckVisitor(result);
        module->accept(&typeCheckVisitor);
    }
}