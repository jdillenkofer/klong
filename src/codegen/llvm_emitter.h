#pragma once

#include "codegen/llvm_emit_visitor.h"
#include "ast/module.h"

namespace klong {
    class LLVMEmitter {
    public:
        void emit(ModulePtr module);

        void printIR();

        std::string getDefaultTargetTriple() const ;

        bool generateObjectFile(const std::string& outputFilename,
                                const std::string& targetTriple,
                                const std::string& cpu = "generic",
                                const std::string& features = "");
    private:
        LLVMEmitVisitor llvmEmitVisitor;
    };
}