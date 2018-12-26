#pragma once

#include "ast/type.h"
#include "ast/visitor.h"

#include "llvm/IR/Type.h"
#include <map>
#include <deque>
#include <llvm/IR/DataLayout.h>

namespace klong {
    class LLVMTypeEmitVisitor : public TypeVisitor {
    public:
        LLVMTypeEmitVisitor(llvm::LLVMContext& context, const llvm::DataLayout dataLayout) :
            _context(context),
            _dataLayout(dataLayout) {

        }

        llvm::Type* getLLVMType(Type* type);

        // Types
        void visitFunctionType(FunctionType* type) override;
        void visitPrimitiveType(PrimitiveType* type) override;
        void visitPointerType(PointerType* type) override;
        void visitCustomType(CustomType *type) override;

    private:
        std::map<std::string, llvm::Type*> _customTypeCache;
        llvm::Type* _valueOfLastType = nullptr;
        std::deque<TypeKind> _outerTypes;
        llvm::LLVMContext& _context;
        llvm::DataLayout _dataLayout;
    };
}