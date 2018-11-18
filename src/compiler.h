#pragma once

#include <string>
#include <iostream>

#include "common/compilation_result.h"
#include "common/option.h"
#include "ast/module.h"
#include "codegen/llvm/llvm_emitter.h"

namespace klong {
    class Compiler {
    public:
        Compiler(Option option) :
            _option(std::move(option)) {
        }

        bool parse(ModulePtr& module, SourceFile &sourceFile);
        bool resolve(ModulePtr& module);
        bool typecheck(ModulePtr& module);
        bool codegen(ModulePtr& module, LLVMEmitter& llvmEmitter, OutputFileType outputFileType);
        bool compile(std::string filepath);
    private:
        void printErrors(CompilationResult& result);
    private:
        CompilationResult _result;
        Option _option;
    };
}