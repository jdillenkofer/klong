#pragma once

#include <string>
#include <iostream>

#include "common/compilation_session.h"
#include "common/option.h"
#include "ast/module.h"
#include "codegen/llvm/llvm_emitter.h"

namespace klong {
    class Compiler {
    public:
        explicit Compiler(Option option) :
            _option(std::move(option)), 
			_session(_option.emitDebugInfo, _option.emitDwarf) {
        }

        bool parse(ModulePtr& module, std::shared_ptr<SourceFile> sourceFile);
        bool resolve(ModulePtr& module);
        bool typecheck(ModulePtr& module);
        bool codegen(ModulePtr& module, LLVMEmitter& llvmEmitter, OutputFileType outputFileType);
        bool compile(std::string filepath);
    private:
        void printResult(CompilationResult &result);
    private:
        CompilationSession _session;
        Option _option;
    };
}