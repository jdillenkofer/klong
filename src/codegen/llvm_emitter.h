#pragma once

#include "codegen/llvm_emit_visitor.h"
#include "ast/module.h"

#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"

namespace klong {
    class LLVMEmitter {
    public:
        void emit(Module& module) {
            module.accept(&llvmEmitVisitor);
        }

        void printIR() {
            auto module = llvmEmitVisitor.getModule();
            if (module != nullptr) {
                module->print(llvm::outs(), nullptr);
            }
        }

        bool generateObjectFile(const std::string& outputFilename,
                                const std::string& targetTriple = llvm::sys::getDefaultTargetTriple(),
                                const std::string& cpu = "generic",
                                const std::string& features = "") {
            auto module = llvmEmitVisitor.getModule();

            if (module == nullptr) {
                return false;
            }

            std::string error;
            auto target = llvm::TargetRegistry::lookupTarget(targetTriple, error);

            llvm::TargetOptions opt;
            auto rm = llvm::Optional<llvm::Reloc::Model>();
            auto targetMachine = target->createTargetMachine(targetTriple, cpu, features, opt, rm);

            module->setTargetTriple(targetTriple);
            module->setDataLayout(targetMachine->createDataLayout());

            std::error_code error_code;
            llvm::raw_fd_ostream destination(outputFilename, error_code, llvm::sys::fs::F_None);

            llvm::legacy::PassManager pass;
            auto fileType = llvm::TargetMachine::CGFT_ObjectFile;

            if (targetMachine->addPassesToEmitFile(pass, destination, nullptr, fileType)) {
                llvm::errs() << "TheTargetMachine can't emit a file of this type";
                return false;
            }

            pass.run(*module);
            destination.flush();
            return true;
        }
    private:
        LLVMEmitVisitor llvmEmitVisitor;
    };
}