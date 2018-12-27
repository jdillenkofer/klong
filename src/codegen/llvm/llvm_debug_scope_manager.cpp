#include "llvm_debug_scope_manager.h"

namespace klong {

	void LLVMDebugScopeManager::push(llvm::DIScope* scope) {
		_debugBlocks.push_back(scope);
	}

	void LLVMDebugScopeManager::pop() {
		_debugBlocks.pop_back();
	}

	void LLVMDebugScopeManager::setDebugFile(llvm::DIFile* debugFile) {
		_debugFile = debugFile;
	}

	llvm::DIFile* LLVMDebugScopeManager::getDebugFile() {
		return _debugFile;
	}

	llvm::DIScope* LLVMDebugScopeManager::getDebugScope() {
		llvm::DIScope* scope = _debugFile;
		if (!_debugBlocks.empty()) {
			scope = _debugBlocks.back();
		}
		return scope;
	}
}