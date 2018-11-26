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

        std::vector<ModulePtr> modules() {
            std::vector<ModulePtr> modules;
            for(auto& item : _allModules) {
                modules.push_back(item.second);
            }
            return modules;
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
                _globalScope.insert(std::pair(name, symbolInfo));
            }
            return unused;
        }

        bool declareType(std::string name, TypeDeclaration* typeDeclaration) {
            if (_typeDeclarations.find(name) != _typeDeclarations.end()) {
                return false;
            }
            _typeDeclarations.insert(std::pair(name, typeDeclaration));
            return true;
        }

        TypeDeclaration* findTypeDeclaration(std::string name) {
            return (*_typeDeclarations.find(name)).second;
        }

    private:
        std::map<std::string, TypeDeclaration*> _typeDeclarations;
        std::map<std::string, SymbolInfo> _globalScope;
        std::map<std::string, std::shared_ptr<Module>> _allModules;
        CompilationResult _result;
    };
}