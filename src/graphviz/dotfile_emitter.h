#pragma once

#include <iostream>
#include <string>

#include "ast/module.h"
#include "graphviz/dotfile_visitor.h"

namespace klong {
    class DotfileEmitter {
    public:
        void emit(const std::string& filename, Module& module) {
            auto graphvizVisitor = DotfileVisitor();
            module.accept(&graphvizVisitor);
            auto output = graphvizVisitor.getDotfileOutput();
            std::cout << output << std::endl;
        }
    };
}