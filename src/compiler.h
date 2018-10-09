#pragma once

#include <string>

#include "common/option.h"
#include "ast/module.h"
#include "codegen/llvm_emitter.h"

namespace klong {
    class Compiler {
    public:
        Compiler(Option option) : _option(option) {
        }

        bool parse(ModulePtr& module, SourceFile &sourceFile);
        bool resolve(ModulePtr& module);
        bool typecheck(ModulePtr& module);
        bool codegen(ModulePtr& module, LLVMEmitter& llvmEmitter);
        bool compile(std::string filepath);
    private:
        Option _option;
    };
}