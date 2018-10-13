#pragma once

#include <fstream>
#include <string>

#include "ast/module.h"
#include "graphviz/dotfile_visitor.h"

namespace klong {
    class DotfileEmitter {
    public:
        bool emit(const std::string& filename, ModulePtr module);
    };
}