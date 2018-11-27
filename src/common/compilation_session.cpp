#include "compilation_session.h"

namespace klong {

    CompilationResult& CompilationSession::getResult() {
        return _result;
    }

    bool CompilationSession::hasModule(const std::string& modulepath) {
        return _allModules.find(modulepath) != _allModules.end();
    }

    void CompilationSession::reserveModule(std::string modulepath) {
        if (hasModule(modulepath)) {
            return;
        }
        _allModules.insert(std::pair(std::move(modulepath), nullptr));
    }

    void CompilationSession::addModule(std::string modulepath, ModulePtr module) {
        _allModules.erase(modulepath);
        _allModules.insert(std::pair(std::move(modulepath), std::move(module)));
    }

    std::vector<ModulePtr> CompilationSession::modules() {
        std::vector<ModulePtr> modules;
        for(auto& item : _allModules) {
            modules.push_back(item.second);
        }
        return modules;
    }

    std::optional<SymbolInfo> CompilationSession::findSymbol(const std::string &name) {
        auto symbol = _globalScope.find(name);
        if (symbol != _globalScope.end()) {
            return (*symbol).second;
        }
        return {};
    }

    bool CompilationSession::declareSymbol(std::string name, SymbolInfo symbolInfo) {
        auto unused = _globalScope.find(name) == _globalScope.end();
        if (unused) {
            _globalScope.insert(std::pair(name, symbolInfo));
        }
        return unused;
    }

    bool CompilationSession::declareType(std::string name, TypeDeclaration* typeDeclaration) {
        if (_typeDeclarations.find(name) != _typeDeclarations.end()) {
            return false;
        }
        _typeDeclarations.insert(std::pair(name, typeDeclaration));
        return true;
    }

    TypeDeclaration* CompilationSession::findTypeDeclaration(std::string name) {
        return (*_typeDeclarations.find(name)).second;
    }
}