#include "resolver.h"

namespace klong {
    Result<ModulePtr, ResolveException> Resolver::resolve(ModulePtr module) {
        auto resolveVisitor = ResolveVisitor();
        module->accept(&resolveVisitor);
        return resolveVisitor.getResult();
    }
}