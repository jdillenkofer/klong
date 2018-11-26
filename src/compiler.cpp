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

    bool Compiler::parse(ModulePtr& module, std::shared_ptr<SourceFile> sourceFile) {
        auto lexer = Lexer(std::move(sourceFile));
        auto parser = Parser(&lexer, &_result);
        parser.parse();
        if (_result.hasErrors()) {
            return false;
        }
        module = _result.success();
        return true;
    }

    bool Compiler::resolve(ModulePtr &module) {
        Resolver resolver;
        resolver.resolve(module, &_result);
        return !_result.hasErrors();
    }

    bool Compiler::typecheck(ModulePtr &module) {
        TypeChecker typeChecker;
        typeChecker.check(module, &_result);
        return !_result.hasErrors();
    }

    bool Compiler::codegen(ModulePtr& module, LLVMEmitter& llvmEmitter, OutputFileType outputFileType) {
        llvmEmitter.emit(module);

        auto filename = module->filenameWithoutExtension();
        if (_option.disableLinking && _option.useCustomOutputPath) {
            filename = _option.customOutputPath;
        }

        llvmEmitter.writeToFile(filename, outputFileType);
        return true;
    }

    void Compiler::printErrors(CompilationResult& result) {
        auto errors = result.getErrors();
        for (uint64_t i = 0; i < errors.size(); i++) {
            auto& error = errors[i];
            auto sourceRange = error.sourceRange();
            if (i == 0 && sourceRange.valid()) {
                std::cout << "file: " << sourceRange.start.filename() << std::endl;
            }
            std::cout << "line " << sourceRange.start.line() << ": " << error.what() << std::endl;
            std::cout << sourceRange.getRelevantSourceText() << std::flush;
        }
    }

    bool Compiler::compile(std::string filepath) {
        auto sourceFile = std::make_shared<SourceFile>(std::move(filepath));
        auto result = sourceFile->loadFromFile();
        if (!result) {
            std::cout << "Cannot load source file " << sourceFile->absolutepath() << std::endl;
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
                printErrors(_result);
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
                printErrors(_result);
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
                printErrors(_result);
                return false;
            }
        }

        /* CODEGEN */
        LLVMEmitter::init();
        defer(LLVMEmitter::destroy());

        auto targetTriple = LLVMEmitter::getDefaultTargetTriple();
        if (_option.isCustomTarget) {
            targetTriple = _option.customTarget;
        }

        LLVMEmitter llvmEmitter(targetTriple);
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

            auto outputFileType = _option.emitAssemblyFile ? OutputFileType::ASM : OutputFileType::OBJECT;
            if (!codegen(module, llvmEmitter, outputFileType)) {
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