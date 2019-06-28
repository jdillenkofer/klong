#include "c_emitter.h"

#include <iostream>

namespace klong {
    void CEmitter::emit(ModulePtr module, CompilationSession* session) {
        _cEmitVisitor->setSession(session);
        module->accept(_cEmitVisitor.get());
    }

    bool CEmitter::writeToFile(const std::string& filename) {
        std::ofstream out(filename + ".c");
        out << _cEmitVisitor->getOutput();
        out.close();
        return !!out;
    }
}