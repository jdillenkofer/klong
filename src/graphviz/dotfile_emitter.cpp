#include "dotfile_emitter.h"

namespace klong {
    bool DotfileEmitter::emit(const std::string& filename, ModulePtr module) {
        auto graphvizVisitor = DotfileVisitor();
        module->accept(&graphvizVisitor);
        auto output = graphvizVisitor.getDotfileOutput();
        std::ofstream out(filename);
        if (!out) {
            return false;
        }
        out << output;
        out.close();
        return true;
    }
}