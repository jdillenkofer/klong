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

    bool CompilationSession::isCyclicDependency(std::string modulepath) {
        if (hasModule(modulepath)) {
            return _allModules[modulepath] == nullptr;
        }
        return false;
    }

    ModulePtr CompilationSession::getModule(std::string modulepath) {
        if (hasModule(modulepath)) {
            return _allModules[modulepath];
        }
        assert(false);
        return nullptr;
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

    void CompilationSession::completeResolved(const std::string& modulepath) {
        if (!isResolved(modulepath)) {
            _resolvedModules.insert(modulepath);
        }
    }

    bool CompilationSession::isResolved(const std::string& modulepath) {
        return std::find(_resolvedModules.begin(), _resolvedModules.end(), modulepath) != _resolvedModules.end();
    }

    void CompilationSession::completeTypechecked(const std::string& modulepath) {
        if (!isTypechecked(modulepath)) {
            _typecheckedModules.insert(modulepath);
        }
    }

    bool CompilationSession::isTypechecked(const std::string& modulepath) {
        return std::find(_typecheckedModules.begin(), _typecheckedModules.end(), modulepath) != _typecheckedModules.end();
    }

	bool CompilationSession::emitDebugInfo() const {
		return _emitDebugInfo;
	}

	bool CompilationSession::emitDwarf() const {
		return _emitDwarf;
	}
}