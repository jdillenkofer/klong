#pragma once

#include "ast/stmt.h"
#include "common/compilation_result.h"
#include "common/symbol_info.h"

#include <map>
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

        bool hasModule(const std::string& modulepath);
        void reserveModule(std::string modulepath);
        void addModule(std::string modulepath, ModulePtr module);
        bool isCyclicDependency(std::string modulepath);
        ModulePtr getModule(std::string modulepath);

        std::vector<ModulePtr> modules();

        std::optional<SymbolInfo> findSymbol(const std::string &name);
        bool declareSymbol(std::string name, SymbolInfo symbolInfo);

        bool declareType(std::string name, TypeDeclaration* typeDeclaration);
        TypeDeclaration* findTypeDeclaration(std::string name);

        void completeResolved(const std::string& modulepath);
        bool isResolved(const std::string& modulepath);
        void completeTypechecked(const std::string& modulepath);
        bool isTypechecked(const std::string& modulepath);

		bool emitDebugInfo() const;
		bool emitDwarf() const;
    private:
        std::map<std::string, TypeDeclaration*> _typeDeclarations;
        std::map<std::string, SymbolInfo> _globalScope;
        std::map<std::string, std::shared_ptr<Module>> _allModules;
        std::set<std::string> _resolvedModules;
        std::set<std::string> _typecheckedModules;
        CompilationResult _result;
		bool _emitDebugInfo;
		bool _emitDwarf;
    };
}