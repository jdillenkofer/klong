#pragma once

#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/Support/TargetSelect.h"

#include "llvm_emit_visitor.h"
#include "common/compilation_session.h"
#include "ast/module.h"

namespace klong {

    enum class OutputFileType {
        OBJECT,
        ASM
    };

    class LLVMEmitter {
    public:
        static void init();

        explicit LLVMEmitter(std::string targetTriple,
                             const std::string& cpu = "generic",
                             const std::string& features = "") :
                             _targetTriple(std::move(targetTriple)) {
            std::string error;
            auto target = llvm::TargetRegistry::lookupTarget(_targetTriple, error);

            llvm::TargetOptions opt;

            // use position independent code - only works with executables
            auto rm = llvm::Reloc::Model::PIC_;

            _targetMachine = target->createTargetMachine(_targetTriple, cpu, features, opt, rm);
            _dataLayout = _targetMachine->createDataLayout();
            _llvmEmitVisitor = std::make_unique<LLVMEmitVisitor>(_dataLayout);
        }

        void emit(ModulePtr module, CompilationSession* session);

        void printIR();

        static std::string getDefaultTargetTriple() ;

        bool writeToFile(const std::string& filename, OutputFileType outputType);

        static void destroy();
    private:
        static bool _initialized;
        std::string _targetTriple;
        llvm::TargetMachine* _targetMachine;
        llvm::DataLayout _dataLayout = llvm::DataLayout("");
        std::unique_ptr<LLVMEmitVisitor> _llvmEmitVisitor;
    };
}