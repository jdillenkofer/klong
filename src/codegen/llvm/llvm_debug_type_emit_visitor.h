#pragma once

#include "ast/type.h"
#include "ast/visitor.h"

#include "llvm_debug_scope_manager.h"

#include "llvm/IR/DIBuilder.h"
#include <map>
#include <deque>
#include <llvm/IR/DataLayout.h>

namespace klong {
	class LLVMDebugTypeEmitVisitor : public TypeVisitor {
	public:
		LLVMDebugTypeEmitVisitor(llvm::LLVMContext& context, const llvm::DataLayout dataLayout) :
			_context(context),
			_dataLayout(dataLayout),
			_debugInfoBuilder(nullptr) {

		}

		llvm::DIType* getLLVMDebugType(Type* type);

		// Types
		void visitFunctionType(FunctionType* type) override;
		void visitPrimitiveType(PrimitiveType* type) override;
		void visitPointerType(PointerType* type) override;
		void visitCustomType(CustomType *type) override;

		void setDebugInfoBuilder(llvm::DIBuilder* debugInfoBuilder);
		void setDebugScopeManager(LLVMDebugScopeManager* debugScopeManager);
	private:
		std::map<std::string, llvm::DIType*> _customTypeCache;
		llvm::DIType* _valueOfLastDebugType = nullptr;
		std::deque<TypeKind> _outerTypes;
		llvm::LLVMContext& _context;
		llvm::DataLayout _dataLayout;
		llvm::DIBuilder* _debugInfoBuilder;
		LLVMDebugScopeManager* _debugScopeManager;
	};
}