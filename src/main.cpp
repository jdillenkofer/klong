#include <iostream>
#include <vector>

#include "common/source_file.h"
#include "lexer/lexer.h"

using namespace klong;

int main(int argc, char* argv[]) {
    // TODO: commandline args
    if (argc != 2) {
        std::cerr << "No input file!" << std::endl;
        return 1;
    }

    const std::string filename = argv[1];
    auto sourceFile = SourceFile(filename);
    auto result = sourceFile.load();
    if (!result) {
        std::cerr << "Cannot load source file " << sourceFile.path() << std::endl;
        return 1;
    }
    auto lexer = Lexer(sourceFile);
    std::vector<Token> tokens;
    while(lexer.hasNext()) {
        auto token = lexer.next();
        tokens.emplace_back(token);
    }
    return 0;
}