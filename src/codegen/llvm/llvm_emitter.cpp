#include "llvm_emitter.h"

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

    void LLVMEmitter::emit(ModulePtr module, CompilationSession* session) {
        _llvmEmitVisitor->setSession(session);
        module->accept(_llvmEmitVisitor.get());
    }

    void LLVMEmitter::printIR() {
        auto module = _llvmEmitVisitor->getModule();
        if (module != nullptr) {
            module->print(llvm::outs(), nullptr);
        }
    }

    std::string LLVMEmitter::getDefaultTargetTriple() {
        return llvm::sys::getDefaultTargetTriple();
    }

    bool LLVMEmitter::writeToFile(const std::string& filename,
                                         OutputFileType outputType) {
        auto module = _llvmEmitVisitor->getModule();

        if (module == nullptr) {
            return false;
        }

        std::string filenameWithExt = filename;
        llvm::TargetMachine::CodeGenFileType fileType;
        if (outputType == OutputFileType::OBJECT) {
            fileType = llvm::TargetMachine::CGFT_ObjectFile;
            filenameWithExt += ".o";
        } else {
            fileType = llvm::TargetMachine::CGFT_AssemblyFile;
            filenameWithExt += ".S";
        }

        std::error_code error_code;
        llvm::raw_fd_ostream destination(filenameWithExt, error_code, llvm::sys::fs::F_None);

        llvm::legacy::PassManager pass;

        module->setTargetTriple(_targetTriple);
        module->setDataLayout(_dataLayout);

        if (_targetMachine->addPassesToEmitFile(pass, destination, nullptr, fileType)) {
            llvm::errs() << "TheTargetMachine can't emit a file of this type";
            return false;
        }

        pass.run(*module);
        destination.flush();
        return true;
    }

    void LLVMEmitter::destroy() {
        if (_initialized) {
            // TODO: this segfaults sometimes!
            // llvm::llvm_shutdown();
            _initialized = false;
        }
    }
}