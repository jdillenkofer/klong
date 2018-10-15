#pragma once

#include "ast/type.h"
#include "ast/visitor.h"

#include "llvm/IR/Type.h"

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
        void visitSimpleType(SimpleType *type) override;
    private:
        llvm::Type* _valueOfLastType = nullptr;
        TypeKind _outerType = TypeKind::PRIMITIVE;
        llvm::LLVMContext& _context;
    };
}