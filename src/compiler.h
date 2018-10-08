#pragma once

#include <string>

#include "common/option.h"

namespace klong {
    class Compiler {
    public:
        Compiler(Option option) : _option(option) {
        }

        bool compile(std::string filepath);
    private:
        Option _option;
    };
}