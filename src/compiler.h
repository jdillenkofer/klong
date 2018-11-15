#pragma once

#include <string>
#include <iostream>

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
        Option _option;
    };

    template<typename T> void printErrors(std::vector<T>& errors) {
        for (uint64_t i = 0; i < errors.size(); i++) {
            auto& error = errors[i];
            auto sourceRange = error.sourceRange();
            if (i == 0 && sourceRange.valid()) {
                std::cout << "file: " << sourceRange.start.filename() << std::endl;
            }
            std::cout << "line " << sourceRange.start.line() << ": " << error.what() << std::endl;
            std::cout << sourceRange.getRelevantSourceText() << std::flush;
        }
    }
}