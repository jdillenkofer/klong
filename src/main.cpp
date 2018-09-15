#include <iostream>

#include "common/source_file.h"
#include "lexer/lexer.h"
#include "parser/parser.h"
#include "parser/resolver.h"
#include "parser/type_checker.h"
#include "terp/treewalk_terp.h"
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
    auto lexer = Lexer(&sourceFile);
    auto parser = Parser(&lexer);
    auto module = parser.parse();
    auto resolver = Resolver();
    module->accept(&resolver);
    auto typeChecker = TypeChecker();
    module->accept(&typeChecker);
    auto interpreter = TreewalkTerp();
    module->accept(&interpreter);
//    auto llvmEmitter = LLVMEmitter();
//    module->accept(&llvmEmitter);
//    llvmEmitter.module()->print(llvm::outs(), nullptr);
    return 0;
}