#include "llvm_emitter.h"

#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/Support/TargetSelect.h"

namespace klong {

    bool LLVMEmitter::_initialized = false;

    void LLVMEmitter::init() {
        if (!_initialized) {
            llvm::InitializeAllTargetInfos();
            llvm::InitializeAllTargets();
            llvm::InitializeAllTargetMCs();
            llvm::InitializeAllAsmParsers();
            llvm::InitializeAllAsmPrinters();
            _initialized = true;
        }
    }

    void LLVMEmitter::emit(ModulePtr module) {
        module->accept(&llvmEmitVisitor);
    }

    void LLVMEmitter::printIR() {
        auto module = llvmEmitVisitor.getModule();
        if (module != nullptr) {
            module->print(llvm::outs(), nullptr);
        }
    }

    std::string LLVMEmitter::getDefaultTargetTriple() const {
        return llvm::sys::getDefaultTargetTriple();
    }

    bool LLVMEmitter::generateObjectFile(const std::string& outputFilename,
                                         const std::string& targetTriple,
                                         const std::string& cpu,
                                         const std::string& features) {
        auto module = llvmEmitVisitor.getModule();

        if (module == nullptr) {
            return false;
        }

        std::string error;
        auto target = llvm::TargetRegistry::lookupTarget(targetTriple, error);

        llvm::TargetOptions opt;

        // use position independent code - only works with executables
        auto rm = llvm::Reloc::Model::PIC_;

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

    void LLVMEmitter::destroy() {
        if (_initialized) {
            llvm::llvm_shutdown();
            _initialized = false;
        }
    }
}