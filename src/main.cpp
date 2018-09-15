#include <iostream>
#include <chrono>

#include "common/source_file.h"
#include "lexer/lexer.h"
#include "parser/parser.h"
#include "parser/resolver.h"
#include "parser/type_checker.h"
#include "terp/treewalk_terp.h"
#include "codegen/LLVMEmitter.h"

#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"

using namespace klong;

int main(int argc, char* argv[]) {
    // TODO: commandline args
    if (argc != 2) {
        std::cerr << "No input file!" << std::endl;
        return 1;
    }

    const std::string filename = argv[1];
    auto sourceFile = SourceFile(filename);
    auto result = sourceFile.loadFromFile();
    if (!result) {
        std::cerr << "Cannot load source file " << sourceFile.path() << std::endl;
        return 1;
    }

    auto parseStart = std::chrono::high_resolution_clock::now();
    auto lexer = Lexer(&sourceFile);
    auto parser = Parser(&lexer);
    auto module = parser.parse();
    auto parseEnd = std::chrono::high_resolution_clock::now();
    std::cout << "Parsing time: " <<
        std::chrono::duration_cast<std::chrono::milliseconds>(parseEnd - parseStart).count() << "ms" << std::endl;

    auto resolveStart = std::chrono::high_resolution_clock::now();
    auto resolver = Resolver();
    module->accept(&resolver);
    auto resolveEnd = std::chrono::high_resolution_clock::now();
    std::cout << "Resolve time: " <<
              std::chrono::duration_cast<std::chrono::milliseconds>(resolveEnd - resolveStart).count() << "ms" << std::endl;

    auto typeCheckStart = std::chrono::high_resolution_clock::now();
    auto typeChecker = TypeChecker();
    module->accept(&typeChecker);
    auto typeCheckEnd = std::chrono::high_resolution_clock::now();
    std::cout << "Typecheck time: " <<
              std::chrono::duration_cast<std::chrono::milliseconds>(typeCheckEnd - typeCheckStart).count() << "ms" << std::endl;

//    auto interpreter = TreewalkTerp();
//    module->accept(&interpreter);

    auto llvmEmissionStart = std::chrono::high_resolution_clock::now();
    auto llvmEmitter = LLVMEmitter();
    module->accept(&llvmEmitter);
    llvmEmitter.module()->print(llvm::outs(), nullptr);

    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmParsers();
    llvm::InitializeAllAsmPrinters();

    auto targetTriple = llvm::sys::getDefaultTargetTriple();

    std::string error;
    auto target = llvm::TargetRegistry::lookupTarget(targetTriple, error);

    auto cpu = "generic";
    auto features = "";

    llvm::TargetOptions opt;
    auto rm = llvm::Optional<llvm::Reloc::Model>();
    auto targetMachine = target->createTargetMachine(targetTriple, cpu, features, opt, rm);

    llvmEmitter.module()->setTargetTriple(targetTriple);
    llvmEmitter.module()->setDataLayout(targetMachine->createDataLayout());

    auto outFilename = "output.o";
    std::error_code error_code;
    llvm::raw_fd_ostream dest(outFilename, error_code, llvm::sys::fs::F_None);

    llvm::legacy::PassManager pass;
    auto fileType = llvm::TargetMachine::CGFT_ObjectFile;

    if (targetMachine->addPassesToEmitFile(pass, dest, fileType)) {
        llvm::errs() << "TheTargetMachine can't emit a file of this type";
        return 1;
    }

    pass.run(*llvmEmitter.module());
    dest.flush();

    auto llvmEmissionEnd = std::chrono::high_resolution_clock::now();
    std::cout << "LLVM time: " <<
              std::chrono::duration_cast<std::chrono::milliseconds>(llvmEmissionEnd - llvmEmissionStart).count() << "ms" << std::endl;

    std::cout << "overall time: " <<
              std::chrono::duration_cast<std::chrono::milliseconds>(llvmEmissionEnd - parseStart).count() << "ms" << std::endl;
    return 0;
}