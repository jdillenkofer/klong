#include "compiler.h"

#include <cstdio>
#include <chrono>

#include "common/defer.h"
#include "common/source_file.h"
#include "lexer/lexer.h"
#include "parser/parser.h"
#include "resolver/resolver.h"
#include "typechecker/typechecker.h"
#include "linker/linker.h"
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
        } else {
            filename = _session.registerAndReturnUniqueObjectFilepath(filename);
        }

        llvmEmitter.writeToFile(filename, outputFileType);
        return true;
    }

    bool compareIncidents(const CompilationIncident& a, const CompilationIncident& b) {
        const auto& aType = a.type;
        const auto& bType = b.type;
        if (aType != bType) {
            return aType < bType;
        }
        const auto& aSourceRange = a.sourceRange;
        const auto& bSourceRange = b.sourceRange;
        // if both sourceRanges are valid, first order by filename then order by line
        if (aSourceRange.valid() && bSourceRange.valid()) {
            std::string aFilename = aSourceRange.start.filename();
            std::string bFilename = bSourceRange.start.filename();
            auto aLine = aSourceRange.start.line();
            auto bLine = bSourceRange.start.line();
            return (aFilename < bFilename) || ((aFilename == bFilename) && (aLine < bLine));
        } else if (bSourceRange.valid()) {
            return true;
        } else if (aSourceRange.valid()) {
            return false;
        }
        // if both sourceranges are invalid order by pointer value (arbitrary)
        return &a < &b;
    }

    void Compiler::reportCompilationResult(CompilationResult &result) {
        auto incidents = result.getIncidents();
        std::sort(incidents.begin(), incidents.end(), compareIncidents);

        std::string prevFilename = "";
        for (uint64_t i = 0; i < incidents.size(); i++) {
            auto& incident = incidents[i];
            auto sourceRange = incident.sourceRange;
            if (sourceRange.valid() && prevFilename != sourceRange.start.filename()) {
                printf("%s - file: %s\n", incident.getTypeName(), sourceRange.start.filename().c_str());
            }
            if (sourceRange.valid()) {
                printf("line %d: %s\n", sourceRange.start.line(), incident.message.c_str());
                printf(sourceRange.getRelevantSourceText().c_str());
            } else {
                printf("%s\n", incident.message.c_str());
            }
            prevFilename = sourceRange.valid() ? sourceRange.start.filename() : "";
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
                reportCompilationResult(_session.getResult());
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
                reportCompilationResult(_session.getResult());
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
                reportCompilationResult(_session.getResult());
                return false;
            } else {
                if (_session.getResult().hasAnyReportableIncidents()) {
                    reportCompilationResult(_session.getResult());
                }
            }
        }

        /* GRAPHVIZ */
        if (_option.emitDotFile) {
            DotfileEmitter graphvizDotFileEmitter;
            graphvizDotFileEmitter.emit(rootModule->filenameWithoutExtension() + ".dot", rootModule);
        }

        /* CODEGEN */
        LLVMEmitter::init();
        defer(
            // @Robustness: Something is weird with llvms memory management.
            // If I call llvm::shutdown while llvm stuff is still around it crashes
            // For now we never call llvm::llvm_shutdown and let the operating system free the memory
            // THIS COULDN'T GET REPRODUCED WITH LLVM 8, MAYBE IT'S FIXED NOW
            LLVMEmitter::destroy();
        );

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
                    }
            );

            if (!(_option.disableLinking && _option.useCustomOutputPath)) {
                std::filesystem::create_directory("obj");
            }

            for (auto& module : _session.modules()) {
                LLVMEmitter llvmEmitter(targetTriple);
                auto outputFileType = _option.emitAssemblyFile ? OutputFileType::ASM : OutputFileType::OBJECT;
                if (!codegen(module, llvmEmitter, outputFileType)) {
                    return false;
                }

                if (_option.printIR) {
                    llvmEmitter.printIR();
                }
            }
        }

        /* LINKING */
        if (!_option.disableLinking) {
            auto linkStart = std::chrono::high_resolution_clock::now();
            defer(
                if (_option.verbose) {
                    auto linkEnd = std::chrono::high_resolution_clock::now();
                    std::cout << "Link time: " <<
                        std::chrono::duration_cast<std::chrono::milliseconds>(
                            linkEnd - linkStart).count() <<
                        "ms" << std::endl;
                }
            );

            auto objPaths = _session.getObjectFilenames();
            if (!link(objPaths, _option.useCustomOutputPath ? _option.customOutputPath : "a.out", _option.emitDebugInfo)) {
                std::cout << "Linking failed.";
                return false;
            }
        }

        defer(
            if (_option.verbose) {
                auto overallEnd = std::chrono::high_resolution_clock::now();
                std::cout << "overall time: " <<
                    std::chrono::duration_cast<std::chrono::milliseconds>(
                        overallEnd - parseStart).count() <<
                    "ms" << std::endl;
            }
        );

        return true;
    }
}