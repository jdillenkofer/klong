#include "compiler.h"

#include <iostream>
#include <chrono>

#include "common/defer.h"
#include "common/source_file.h"
#include "lexer/lexer.h"
#include "parser/parser.h"
#include "resolver/resolver.h"
#include "typechecker/typechecker.h"
#include "graphviz/dotfile_emitter.h"

namespace klong {

    bool Compiler::parse(ModulePtr& module, SourceFile &sourceFile) {
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
        return true;
    }

    bool Compiler::resolve(klong::ModulePtr &module) {
        Resolver resolver;
        auto resolveResult = resolver.resolve(module);
        if (resolveResult.hasErrors()) {
            for (auto& resolveError : resolveResult.getErrors()) {
                std::cerr << resolveError.what() << std::endl;
            }
            return false;
        }
        return true;
    }

    bool Compiler::typecheck(klong::ModulePtr &module) {
        TypeChecker typeChecker;
        auto typeCheckResult = typeChecker.check(module);
        if (typeCheckResult.hasErrors()) {
            for (auto& typeCheckError: typeCheckResult.getErrors()) {
                std::cerr << typeCheckError.what() << std::endl;
            }
            return false;
        }
        return true;
    }

    bool Compiler::codegen(ModulePtr& module, LLVMEmitter& llvmEmitter) {
        llvmEmitter.emit(module);

        auto objOutputPath = module->filenameWithoutExtension() + ".o";
        if (_option.disableLinking && _option.useCustomOutputPath) {
            objOutputPath = _option.customOutputPath;
        }

        auto targetTriple = llvmEmitter.getDefaultTargetTriple();
        if (_option.isCustomTarget) {
            targetTriple = _option.customTarget;
        }

        llvmEmitter.generateObjectFile(objOutputPath, targetTriple);
        return true;
    }

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

            if (!parse(module, sourceFile)) {
                return false;
            }
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

            if (!resolve(module)) {
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

            if (!typecheck(module)) {
                return false;
            }
        }

        /* CODEGEN */
        LLVMEmitter llvmEmitter;
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


            if (!codegen(module, llvmEmitter)) {
                return false;
            }
        }

        if (_option.verbose) {
            llvmEmitter.printIR();
        }

        /* GRAPHVIZ */
        if (_option.emitDotFile) {
            DotfileEmitter graphvizDotFileEmitter;
            graphvizDotFileEmitter.emit(module->filenameWithoutExtension() + ".dot", module);
        }
        return true;
    }
}