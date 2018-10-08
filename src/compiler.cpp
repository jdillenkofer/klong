#include "compiler.h"

#include <iostream>
#include <chrono>

#include "common/defer.h"
#include "common/source_file.h"
#include "lexer/lexer.h"
#include "parser/parser.h"
#include "resolver/resolver.h"
#include "typechecker/typechecker.h"
#include "codegen/llvm_emitter.h"
#include "graphviz/dotfile_emitter.h"

namespace klong {
    bool Compiler::compile(std::string filepath) {
        auto sourceFile = SourceFile(std::move(filepath));
        auto result = sourceFile.loadFromFile();
        if (!result) {
            std::cerr << "Cannot load source file " << sourceFile.path() << std::endl;
            return false;
        }

        /* PARSING */
        ModulePtr module;
        auto parseStart = std::chrono::high_resolution_clock::now();
        {
            defer(
                if (_option.verbose) {
                    auto parseEnd = std::chrono::high_resolution_clock::now();
                    std::cout << "Parsing time: " <<
                    std::chrono::duration_cast<std::chrono::milliseconds>(parseEnd - parseStart).count() <<
                    "ms" << std::endl;
                }
            );

            auto lexer = Lexer(&sourceFile);
            auto parser = Parser(&lexer);
            auto parseResult = parser.parse();
            if (parseResult.hasErrors()) {
                for (auto& parseError : parseResult.getErrors()) {
                    std::cerr << parseError.what() << std::endl;
                }
                return false;
            }
            module = parseResult.success();
        }


        /* RESOLVING */
        {
            auto resolveStart = std::chrono::high_resolution_clock::now();
            defer(
                if (_option.verbose) {
                    auto resolveEnd = std::chrono::high_resolution_clock::now();
                    std::cout << "Resolve time: " <<
                    std::chrono::duration_cast<std::chrono::milliseconds>(resolveEnd - resolveStart).count() <<
                    "ms" << std::endl;
                }
            );

            auto resolver = Resolver();
            auto resolveResult = resolver.resolve(module);
            if (resolveResult.hasErrors()) {
                for (auto& resolveError : resolveResult.getErrors()) {
                    std::cerr << resolveError.what() << std::endl;
                }
                return false;
            }

        }

        /* TYPECHECKING */
        {
            auto typeCheckStart = std::chrono::high_resolution_clock::now();
            defer(
                if (_option.verbose) {
                    auto typeCheckEnd = std::chrono::high_resolution_clock::now();
                    std::cout << "Typecheck time: " <<
                    std::chrono::duration_cast<std::chrono::milliseconds>(typeCheckEnd - typeCheckStart).count() <<
                    "ms" << std::endl;
                }
            );

            auto typeChecker = TypeChecker();
            auto typeCheckResult = typeChecker.check(module);
            if (typeCheckResult.hasErrors()) {
                for (auto& typeCheckError: typeCheckResult.getErrors()) {
                    std::cerr << typeCheckError.what() << std::endl;
                }
                return false;
            }
        }

        /* CODEGEN */
        auto llvmEmitter = LLVMEmitter();
        {
            auto llvmEmissionStart = std::chrono::high_resolution_clock::now();
            defer(
                if (_option.verbose) {
                    auto llvmEmissionEnd = std::chrono::high_resolution_clock::now();
                    std::cout << "LLVM time: " <<
                    std::chrono::duration_cast<std::chrono::milliseconds>(llvmEmissionEnd - llvmEmissionStart).count() <<
                    "ms" << std::endl;

                    std::cout << "overall time: " <<
                    std::chrono::duration_cast<std::chrono::milliseconds>(llvmEmissionEnd - parseStart).count() <<
                    "ms" << std::endl;
                }
            );
            llvmEmitter.emit(module);
        }

        auto objOutputPath = module->filenameWithoutExtension() + ".o";

        if (_option.disableLinking && _option.useCustomOutputPath) {
            objOutputPath = _option.customOutputPath;
        }

        auto targetTriple = llvmEmitter.getDefaultTargetTriple();
        if (_option.isCustomTarget) {
            targetTriple = _option.customTarget;
        }

        llvmEmitter.generateObjectFile(objOutputPath, targetTriple);

        if (_option.verbose) {
            llvmEmitter.printIR();
        }

        /* GRAPHVIZ */
        if (_option.emitDotFile) {
            auto graphvizDotFileEmitter = DotfileEmitter();
            graphvizDotFileEmitter.emit(module->filenameWithoutExtension() + ".dot", module);
        }
        return true;
    }
}