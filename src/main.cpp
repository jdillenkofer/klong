#include <iostream>

#include "common/option.h"
#include "compiler.h"

using namespace klong;

void printHelp();

int main(int argc, char* argv[]) {
    auto optionResult = parseOptions(argc, argv);
    if (optionResult.isError()) {
        std::cout << optionResult.error() << std::endl;
        return 1;
    }
    auto option = optionResult.success();
    auto entryFile = option.filepath;

    if (option.help) {
        printHelp();
        return 0;
    }

    Compiler compiler(option);
    if (!compiler.compile(entryFile)) {
        return 1;
    }
    return 0;
}

void printHelp() {
    std::cout << "usage klong [options] [@entryfile]" << std::endl;
    std::cout << "Options:" << std::endl;
    std::cout << "\t-c disable linking" << std::endl;
	std::cout << "\t-g Emit debug information" << std::endl;
	std::cout << "\t-d Emit codeview debug information (for windows msvc)" << std::endl;
    std::cout << "\t-o [file] Emits the output in the specified file." << std::endl;
    std::cout << "\t-s Emit the assembly source instead of a binary objectfile." << std::endl;
    std::cout << "\t-b targetTriple" << std::endl;
	std::cout << "\t-i print llvm IR" << std::endl;
    std::cout << "\t-p emit graphviz dot files" << std::endl;
    std::cout << "\t-v (verbose) shows compile times" << std::endl;
    std::cout << "\t-h shows this help" << std::endl;
}