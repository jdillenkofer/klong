#pragma once

#include "ast/type.h"
#include "ast/visitor.h"

#include "llvm/IR/Type.h"
#include "llvm/IR/DIBuilder.h"
#include <map>
#include <deque>
#include <llvm/IR/DataLayout.h>

namespace klong {
    class LLVMTypeEmitVisitor : public TypeVisitor {
    public:
        LLVMTypeEmitVisitor(llvm::LLVMContext& context, const llvm::DataLayout dataLayout) :
            _context(context),
            _dataLayout(dataLayout),
			_debugInfoBuilder(nullptr) {

        }

        llvm::Type* getLLVMType(Type* type);
		llvm::DIType* getLLVMDebugType(Type* type);

        // Types
        void visitFunctionType(FunctionType* type) override;
        void visitPrimitiveType(PrimitiveType* type) override;
        void visitPointerType(PointerType* type) override;
        void visitCustomType(CustomType *type) override;

		void setDebugInfoBuilder(llvm::DIBuilder* debugInfoBuilder);
    private:
        std::map<std::string, llvm::Type*> _customTypeCache;
        llvm::Type* _valueOfLastType = nullptr;
		llvm::DIType* _valueOfLastDebugType = nullptr;
        std::deque<TypeKind> _outerTypes;
        llvm::LLVMContext& _context;
        llvm::DataLayout _dataLayout;
		llvm::DIBuilder* _debugInfoBuilder;
    };
}