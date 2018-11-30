#include "typechecker.h"

namespace klong {
    void TypeChecker::check(ModulePtr module, CompilationSession* session) {
        auto typeCheckVisitor = TypeCheckVisitor(session);
        module->accept(&typeCheckVisitor);
    }
}