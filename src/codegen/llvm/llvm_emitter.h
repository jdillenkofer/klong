#pragma once

#include "llvm_emit_visitor.h"
#include "ast/module.h"

namespace klong {
    class LLVMEmitter {
    public:
        static void init();
        void emit(ModulePtr module);

        void printIR();

        std::string getDefaultTargetTriple() const ;

        bool generateObjectFile(const std::string& outputFilename,
                                const std::string& targetTriple,
                                const std::string& cpu = "generic",
                                const std::string& features = "");

        static void destroy();
    private:
        static bool _initialized;
        LLVMEmitVisitor llvmEmitVisitor;
    };
}