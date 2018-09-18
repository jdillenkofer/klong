#include <iostream>
#include <chrono>

#include "common/source_file.h"
#include "lexer/lexer.h"
#include "parser/parser.h"
#include "parser/resolver.h"
#include "parser/type_checker.h"
#include "codegen/LLVMEmitter.h"

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

    /* PARSING */
    auto parseStart = std::chrono::high_resolution_clock::now();
    auto lexer = Lexer(&sourceFile);
    auto parser = Parser(&lexer);
    auto module = parser.parse();
    auto parseEnd = std::chrono::high_resolution_clock::now();
    std::cout << "Parsing time: " <<
        std::chrono::duration_cast<std::chrono::milliseconds>(parseEnd - parseStart).count() << "ms" << std::endl;

    /* RESOLVING */
    auto resolveStart = std::chrono::high_resolution_clock::now();
    auto resolver = Resolver();
    module->accept(&resolver);
    auto resolveEnd = std::chrono::high_resolution_clock::now();
    std::cout << "Resolve time: " <<
              std::chrono::duration_cast<std::chrono::milliseconds>(resolveEnd - resolveStart).count() << "ms" << std::endl;

    /* TYPECHECKING */
    auto typeCheckStart = std::chrono::high_resolution_clock::now();
    auto typeChecker = TypeChecker();
    module->accept(&typeChecker);
    auto typeCheckEnd = std::chrono::high_resolution_clock::now();
    std::cout << "Typecheck time: " <<
              std::chrono::duration_cast<std::chrono::milliseconds>(typeCheckEnd - typeCheckStart).count() << "ms" << std::endl;

    /* CODEGEN */
    auto llvmEmissionStart = std::chrono::high_resolution_clock::now();
    auto llvmEmitter = LLVMEmitter();
    module->accept(&llvmEmitter);

    llvmEmitter.generateObjectFile("output.o");
    llvmEmitter.printIR();

    auto llvmEmissionEnd = std::chrono::high_resolution_clock::now();
    std::cout << "LLVM time: " <<
              std::chrono::duration_cast<std::chrono::milliseconds>(llvmEmissionEnd - llvmEmissionStart).count() << "ms" << std::endl;

    std::cout << "overall time: " <<
              std::chrono::duration_cast<std::chrono::milliseconds>(llvmEmissionEnd - parseStart).count() << "ms" << std::endl;
    return 0;
}