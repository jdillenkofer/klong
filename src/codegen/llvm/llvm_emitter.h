#pragma once

#include "llvm_emit_visitor.h"
#include "ast/module.h"

namespace klong {

    enum class OutputFileType {
        OBJECT,
        ASM
    };

    class LLVMEmitter {
    public:
        static void init();
        void emit(ModulePtr module);

        void printIR();

        std::string getDefaultTargetTriple() const ;

        bool writeToFile(const std::string& filename,
                                const std::string& targetTriple,
                                OutputFileType outputType = OutputFileType::OBJECT,
                                const std::string& cpu = "generic",
                                const std::string& features = "");

        static void destroy();
    private:
        static bool _initialized;
        LLVMEmitVisitor llvmEmitVisitor;
    };
}