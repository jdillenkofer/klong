#include "resolver.h"

namespace klong {
    void Resolver::resolve(ModulePtr module, CompilationSession* session) {
        auto resolveVisitor = ResolveVisitor(session);
        module->accept(&resolveVisitor);
    }
}