#pragma once

#include "ast/stmt.h"
#include "common/compilation_result.h"
#include "common/symbol_info.h"

#include <map>
#include <vector>
#include <memory>
#include <algorithm>

namespace klong {
    class CompilationSession {
    public:

        CompilationResult& getResult();

        bool hasModule(const std::string& modulepath);
        void reserveModule(std::string modulepath);
        void addModule(std::string modulepath, ModulePtr module);

        std::vector<ModulePtr> modules();

        std::optional<SymbolInfo> findSymbol(const std::string &name);
        bool declareSymbol(std::string name, SymbolInfo symbolInfo);

        bool declareType(std::string name, TypeDeclaration* typeDeclaration);
        TypeDeclaration* findTypeDeclaration(std::string name);

    private:
        std::map<std::string, TypeDeclaration*> _typeDeclarations;
        std::map<std::string, SymbolInfo> _globalScope;
        std::map<std::string, std::shared_ptr<Module>> _allModules;
        CompilationResult _result;
    };
}