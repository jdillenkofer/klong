#pragma once

#include "llvm/IR/DIBuilder.h"

namespace klong {
	class LLVMDebugScopeManager {
	public:
		void push(llvm::DIScope* scope);
		void pop();

		void setDebugFile(llvm::DIFile* debugFile);
		llvm::DIFile* getDebugFile();
		llvm::DIScope* getDebugScope();
	private:
		llvm::DIFile* _debugFile;
		std::vector<llvm::DIScope*> _debugBlocks;
	};
}