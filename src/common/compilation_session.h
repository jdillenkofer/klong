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

        CompilationResult& getResult() {
            return _result;
        }

        bool hasModule(const std::string& modulepath) {
            return _allModules.find(modulepath) != _allModules.end();
        }

        void reserveModule(std::string modulepath) {
            if (hasModule(modulepath)) {
                return;
            }
            _allModules.insert(std::pair(std::move(modulepath), nullptr));
        }

        void addModule(std::string modulepath, ModulePtr module) {
            _allModules.erase(modulepath);
            _allModules.insert(std::pair(std::move(modulepath), std::move(module)));
        }

        std::optional<SymbolInfo> find(const std::string& name) {
            auto symbol = _globalScope.find(name);
            if (symbol != _globalScope.end()) {
                return (*symbol).second;
            }
            return {};
        }

        bool declare(std::string name, SymbolInfo symbolInfo) {
            auto unused = _globalScope.find(name) == _globalScope.end();
            if (unused) {
                auto externalDecl = translateToExternalDeclaration(symbolInfo.declarationStmt);
                symbolInfo.declarationStmt = externalDecl.get();
                _externalDeclarations.push_back(externalDecl);
                _globalScope.insert(std::pair(name, symbolInfo));
            }
            return unused;
        }

        std::vector<std::shared_ptr<ExternalDeclaration>>& externalDeclarations() {
            return _externalDeclarations;
        }

    private:
        std::shared_ptr<ExternalDeclaration> translateToExternalDeclaration(Stmt* stmt) {
            auto function = dynamic_cast<Function*>(stmt);
            auto varDecl = dynamic_cast<VariableDeclaration*>(stmt);
            auto extDecl = dynamic_cast<ExternalDeclaration*>(stmt);
            if (function) {
                return std::make_shared<ExternalDeclaration>(function->sourceRange(),
                        function->name(), std::shared_ptr<Type>(function->functionType()->clone()));
            }
            if (varDecl) {
                // BUG: cannot clone here because the type is not resolved for smth like this:
                // pub const a = 5;
                return std::make_shared<ExternalDeclaration>(varDecl->sourceRange(),
                        varDecl->name(), std::shared_ptr<Type>(varDecl->type()->clone()));
            }
            if (extDecl) {
                return std::make_shared<ExternalDeclaration>(*extDecl);
            }
            assert(false);
            return nullptr;
        }

    private:
        std::vector<std::shared_ptr<ExternalDeclaration>> _externalDeclarations;
        std::map<std::string, SymbolInfo> _globalScope;
        std::map<std::string, std::shared_ptr<Module>> _allModules;
        CompilationResult _result;
    };
}