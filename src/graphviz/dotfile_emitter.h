#pragma once

#include <fstream>
#include <string>

#include "ast/module.h"
#include "graphviz/dotfile_visitor.h"

namespace klong {
    class DotfileEmitter {
    public:
        bool emit(const std::string& filename, Module& module) {
            auto graphvizVisitor = DotfileVisitor();
            module.accept(&graphvizVisitor);
            auto output = graphvizVisitor.getDotfileOutput();
            std::ofstream out(filename);
            if (!out) {
                return false;
            }
            out << output;
            out.close();
            return true;
        }
    };
}