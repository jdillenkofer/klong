#pragma once

#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/JITSymbol.h"
#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/LambdaResolver.h"
#include "llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h"
#include "llvm/ExecutionEngine/RTDyldMemoryManager.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Mangler.h"
#include "llvm/Support/DynamicLibrary.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"

#include "ast/module.h"
#include "codegen/llvm/llvm_emit_visitor.h"

#include <algorithm>
#include <map>
#include <memory>
#include <string>
#include <vector>

namespace klong {

    class KlongJIT {
    public:
        using ObjLayerT = llvm::orc::RTDyldObjectLinkingLayer;
        using CompileLayerT = llvm::orc::IRCompileLayer<ObjLayerT, llvm::orc::SimpleCompiler>;

        KlongJIT():
            _symbolResolver(createLegacyLookupResolver(
            _executionSession,
            [this](const std::string &Name) {
                return _objectLayer.findSymbol(Name, true);
            },
            [](llvm::Error Err) { cantFail(std::move(Err), "lookupFlags failed"); })),
                _targetMachine(llvm::EngineBuilder().selectTarget()), _dataLayout(_targetMachine->createDataLayout()),
                _objectLayer(_executionSession,
                    [this](llvm::orc::VModuleKey) {
                        return ObjLayerT::Resources{
                            std::make_shared<llvm::SectionMemoryManager>(), _symbolResolver};
                    }),
            _compileLayer(_objectLayer, llvm::orc::SimpleCompiler(*_targetMachine)) {
            llvm::sys::DynamicLibrary::LoadLibraryPermanently(nullptr);
        }

        llvm::orc::VModuleKey addModule(std::shared_ptr<klong::Module> module) {
            module->accept(&_llvmEmitVisitor);
            std::unique_ptr<llvm::Module> llvmModule = _llvmEmitVisitor.getModule();
            auto K = _executionSession.allocateVModule();
            cantFail(_compileLayer.addModule(K, std::move(llvmModule)));
            _moduleKeys.push_back(K);
            return K;
        }

        void removeModule(llvm::orc::VModuleKey K) {
            _moduleKeys.erase(llvm::find(_moduleKeys, K));
            cantFail(_compileLayer.removeModule(K));
        }

        llvm::JITSymbol findSymbol(const std::string& symbolName) {
            return findMangledSymbol(mangle(symbolName));
        }

    private:
        std::string mangle(const std::string &Name) {
            std::string MangledName;
            {
                llvm::raw_string_ostream MangledNameStream(MangledName);
                llvm::Mangler::getNameWithPrefix(MangledNameStream, Name, _dataLayout);
            }
            return MangledName;
        }

        llvm::JITSymbol findMangledSymbol(const std::string &Name) {
#ifdef _WIN32
            // The symbol lookup of ObjectLinkingLayer uses the SymbolRef::SF_Exported
            // flag to decide whether a symbol will be visible or not, when we call
            // IRCompileLayer::findSymbolIn with ExportedSymbolsOnly set to true.
            //
            // But for Windows COFF objects, this flag is currently never set.
            // For a potential solution see: https://reviews.llvm.org/rL258665
            // For now, we allow non-exported symbols on Windows as a workaround.
            const bool ExportedSymbolsOnly = false;
#else
            const bool ExportedSymbolsOnly = true;
#endif

            // Search modules in reverse order: from last added to first added.
            // This is the opposite of the usual search order for dlsym, but makes more
            // sense in a REPL where we want to bind to the newest available definition.
            for (auto H : llvm::make_range(_moduleKeys.rbegin(), _moduleKeys.rend()))
                if (auto Sym = _compileLayer.findSymbolIn(H, Name, ExportedSymbolsOnly))
                    return Sym;

            // If we can't find the symbol in the JIT, try looking in the host process.
            if (auto SymAddr = llvm::RTDyldMemoryManager::getSymbolAddressInProcess(Name))
                return llvm::JITSymbol(SymAddr, llvm::JITSymbolFlags::Exported);

#ifdef _WIN32
            // For Windows retry without "_" at beginning, as RTDyldMemoryManager uses
            // GetProcAddress and standard libraries like msvcrt.dll use names
            // with and without "_" (for example "_itoa" but "sin").
            if (Name.length() > 2 && Name[0] == '_')
              if (auto SymAddr =
                      RTDyldMemoryManager::getSymbolAddressInProcess(Name.substr(1)))
                return JITSymbol(SymAddr, JITSymbolFlags::Exported);
#endif

            return nullptr;
        }

        llvm::orc::ExecutionSession _executionSession;
        std::shared_ptr<llvm::orc::SymbolResolver> _symbolResolver;
        std::unique_ptr<llvm::TargetMachine> _targetMachine;
        const llvm::DataLayout _dataLayout;
        ObjLayerT _objectLayer;
        CompileLayerT _compileLayer;
        LLVMEmitVisitor _llvmEmitVisitor;
        std::vector<llvm::orc::VModuleKey> _moduleKeys;
    };
}
