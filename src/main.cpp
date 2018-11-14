#include <iostream>

#include "common/option.h"
#include "compiler.h"

using namespace klong;

void printHelp();

int main(int argc, char* argv[]) {
    auto optionResult = parseOptions(argc, argv);
    if (optionResult.hasErrors()) {
        std::cout << optionResult.getFirstError().value() << std::endl;
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
    std::cout << "\t-o file Place output in file \"file\"." << std::endl;
    std::cout << "\t-b targetTriple" << std::endl;
    std::cout << "\t-d emit graphviz dot files" << std::endl;
    std::cout << "\t-v (verbose) shows compile times" << std::endl;
    std::cout << "\t-h shows this help" << std::endl;
}