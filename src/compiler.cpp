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
        auto path = sourceFile->absolutepath();
        auto lexer = Lexer(std::move(sourceFile));
        auto parser = Parser(&lexer, &_session);
        _session.reserveModule(path);
        parser.parse();
        if (_session.getResult().hasErrors()) {
            return false;
        }
        module = _session.getResult().success();
        _session.addModule(path, module);
        return true;
    }

    bool Compiler::resolve(ModulePtr& module) {
        Resolver resolver;
        resolver.resolve(module, &_session);
        return !_session.getResult().hasErrors();
    }

    bool Compiler::typecheck(ModulePtr &module) {
        TypeChecker typeChecker;
        typeChecker.check(module, &_session);
        return !_session.getResult().hasErrors();
    }

    bool Compiler::codegen(ModulePtr& module, LLVMEmitter& llvmEmitter, OutputFileType outputFileType) {
        llvmEmitter.emit(module, &_session);

        auto filename = module->filenameWithoutExtension();
        if (_option.disableLinking && _option.useCustomOutputPath) {
            filename = _option.customOutputPath;
        }

        llvmEmitter.writeToFile(filename, outputFileType);
        return true;
    }

    void Compiler::printResult(CompilationResult &result) {
        auto warnings = result.getWarnings();
        // TODO: fix warnings from multiple files
        for (uint64_t i = 0; i < warnings.size(); i++) {
            auto& error = warnings[i];
            auto sourceRange = error.sourceRange();
            if (i == 0 && sourceRange.valid()) {
                std::cout << "WARNING - file: " << sourceRange.start.filename() << std::endl;
            }
            std::cout << "line " << sourceRange.start.line() << ": " << error.what() << std::endl;
            std::cout << sourceRange.getRelevantSourceText() << std::flush;
        }
        auto errors = result.getErrors();
        // TODO: fix error messages from multiple files
        for (uint64_t i = 0; i < errors.size(); i++) {
            auto& error = errors[i];
            auto sourceRange = error.sourceRange();
            if (i == 0 && sourceRange.valid()) {
                std::cout << "ERROR - file: " << sourceRange.start.filename() << std::endl;
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
        ModulePtr rootModule;
        auto parseStart = std::chrono::high_resolution_clock::now();
        {
            defer(
                    if (_option.verbose) {
                        auto parseEnd = std::chrono::high_resolution_clock::now();
                        std::cout << "Parsing time: " <<
                                  std::chrono::duration_cast<std::chrono::milliseconds>(parseEnd - parseStart).count()
                                  <<
                                  "ms" << std::endl;
                    }
            );

            if (!parse(rootModule, sourceFile)) {
                printResult(_session.getResult());
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
                                  std::chrono::duration_cast<std::chrono::milliseconds>(
                                          resolveEnd - resolveStart).count() <<
                                  "ms" << std::endl;
                    }
            );

            if (!resolve(rootModule)) {
                printResult(_session.getResult());
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
                                  std::chrono::duration_cast<std::chrono::milliseconds>(
                                          typeCheckEnd - typeCheckStart).count() <<
                                  "ms" << std::endl;
                    }
            );

            if (!typecheck(rootModule)) {
                printResult(_session.getResult());
                return false;
            } else {
                if (_session.getResult().hasWarnings()) {
                    printResult(_session.getResult());
                }
            }
        }

        /* CODEGEN */
        LLVMEmitter::init();
        defer(LLVMEmitter::destroy());

        auto targetTriple = LLVMEmitter::getDefaultTargetTriple();
        if (_option.isCustomTarget) {
            targetTriple = _option.customTarget;
        }

        {
            auto llvmEmissionStart = std::chrono::high_resolution_clock::now();
            defer(
                    if (_option.verbose) {
                        auto llvmEmissionEnd = std::chrono::high_resolution_clock::now();
                        std::cout << "LLVM time: " <<
                                  std::chrono::duration_cast<std::chrono::milliseconds>(
                                          llvmEmissionEnd - llvmEmissionStart).count() <<
                                  "ms" << std::endl;

                        std::cout << "overall time: " <<
                                  std::chrono::duration_cast<std::chrono::milliseconds>(
                                          llvmEmissionEnd - parseStart).count() <<
                                  "ms" << std::endl;
                    }
            );
            for (auto& module : _session.modules()) {
                LLVMEmitter llvmEmitter(targetTriple);
                auto outputFileType = _option.emitAssemblyFile ? OutputFileType::ASM : OutputFileType::OBJECT;
                if (!codegen(module, llvmEmitter, outputFileType)) {
                    return false;
                }

                if (_option.verbose) {
                    llvmEmitter.printIR();
                }
            }
        }

        /* GRAPHVIZ */
        if (_option.emitDotFile) {
            DotfileEmitter graphvizDotFileEmitter;
            graphvizDotFileEmitter.emit(rootModule->filenameWithoutExtension() + ".dot", rootModule);
        }
        return true;
    }
}