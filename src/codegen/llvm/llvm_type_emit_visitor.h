#pragma once

#include "ast/type.h"
#include "ast/visitor.h"

#include "llvm/IR/Type.h"
#include <map>
#include <deque>

namespace klong {
    class LLVMTypeEmitVisitor : public TypeVisitor {
    public:
        LLVMTypeEmitVisitor(llvm::LLVMContext& context) : _context(context) {

        }

        llvm::Type* getLLVMType(Type *type);
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
    };
}