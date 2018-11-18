#include "resolver.h"

namespace klong {
    void Resolver::resolve(ModulePtr module, CompilationResult* result) {
        auto resolveVisitor = ResolveVisitor(result);
        module->accept(&resolveVisitor);
    }
}