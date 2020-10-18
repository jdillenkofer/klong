#pragma once

#include "array.h"
#include "ast/stmt.h"
#include "common/compilation_result.h"
#include "common/symbol_info.h"

#include <map>
#include <mutex>
#include <set>
#include <memory>
#include <algorithm>

namespace klong {
    class CompilationSession {
    public:
		explicit CompilationSession(bool emitDebugInfo, bool emitDwarf):
			_emitDebugInfo(emitDebugInfo), _emitDwarf(emitDwarf) {
		}

        CompilationResult& getResult();
        void reportError(std::string message, SourceRange sourceRange = SourceRange());
        void reportWarning(std::string message, SourceRange sourceRange = SourceRange());
        void reportInfo(std::string message, SourceRange sourceRange = SourceRange());

        bool hasModule(const std::string& modulepath);
        void reserveModule(std::string modulepath);
        void addModule(std::string modulepath, ModulePtr module);
        bool isCyclicDependency(std::string modulepath);
        ModulePtr getModule(std::string modulepath);

        Array<ModulePtr> modules();

        std::optional<SymbolInfo> findSymbol(const std::string &name);
        Array<SymbolInfo> hasPrivateSymbols(const std::string& name);
        bool declareSymbol(std::string name, SymbolInfo symbolInfo);
        void declarePrivateSymbol(std::string name, SymbolInfo symbolInfo);

        bool declareType(std::string name, TypeDeclaration* typeDeclaration);
        TypeDeclaration* findTypeDeclaration(std::string name);

        void completeResolved(const std::string& modulepath);
        bool isResolved(const std::string& modulepath);
        void completeTypechecked(const std::string& modulepath);
        bool isTypechecked(const std::string& modulepath);

		std::string registerAndReturnUniqueObjectFilepath(std::string prefixname);
		Array<std::string> getObjectFilenames();

		bool emitDebugInfo() const;
		bool emitDwarf() const;
    private:
        std::map<std::string, TypeDeclaration*> _typeDeclarations;
        std::map<std::string, SymbolInfo> _globalScope;
        std::multimap<std::string, SymbolInfo> _privateSymbols;
        std::map<std::string, std::shared_ptr<Module>> _allModules;
        std::set<std::string> _resolvedModules;
        std::set<std::string> _typecheckedModules;

		Array<std::string> _objectFilenames;

        CompilationResult _result;
		bool _emitDebugInfo;
		bool _emitDwarf;
    };
}