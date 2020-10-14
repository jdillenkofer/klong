#include <cstdio>

#include "common/option.h"
#include "compiler.h"

using namespace klong;

void printHelp();

int main(int argc, char* argv[]) {
    auto optionResult = parseOptions(argc, argv);
    if (optionResult.isError()) {
        printf("%s\n", optionResult.error().c_str());
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
    printf("usage klong [options] [@entryfile]\n");
    printf("Options:\n");
    printf("\t-c disable linking\n");
    printf("\t-g Emit debug information\n");
    printf("\t-d Emit codeview debug information (for windows msvc)\n");
    printf("\t-o [file] Emits the output in the specified file.\n");
    printf("\t-s Emit the assembly source instead of a binary objectfile.\n");
    printf("\t-b targetTriple\n");
    printf("\t-i print llvm IR\n");
    printf("\t-p emit graphviz dot files\n");
    printf("\t-v (verbose) shows compile times\n");
    printf("\t-h shows this help\n");
}