#include <llvm/IR/DerivedTypes.h>
#include "llvm_type_emit_visitor.h"

namespace klong {

    llvm::Type* LLVMTypeEmitVisitor::getLLVMType(Type *type) {
        type->accept(this);
        return _valueOfLastType;
    }

    void LLVMTypeEmitVisitor::visitFunctionType(FunctionType* type) {

        std::vector<llvm::Type*> paramTypes;

        auto prevOuterType = _outerType;
        _outerType = TypeKind::FUNCTION;

        for (auto& paramType : type->paramTypes()) {
            paramTypes.push_back(getLLVMType(paramType));
        }
        llvm::Type* returnType = getLLVMType(type->returnType());

        _outerType = prevOuterType;

        _valueOfLastType = llvm::FunctionType::get(returnType, paramTypes, false);
    }

    void LLVMTypeEmitVisitor::visitPrimitiveType(PrimitiveType *type) {
        switch (type->type()) {
            case PrimitiveTypeKind::VOID:
            {
                if (_outerType == TypeKind::POINTER) {
                    _valueOfLastType = llvm::Type::getInt8Ty(_context);
                    break;
                }
                _valueOfLastType = llvm::Type::getVoidTy(_context);
                break;
            }
            case PrimitiveTypeKind::BOOL:
                _valueOfLastType = llvm::Type::getInt1Ty(_context);
                break;
            case PrimitiveTypeKind::I8:
            case PrimitiveTypeKind::U8:
                _valueOfLastType = llvm::Type::getInt8Ty(_context);
                break;
            case PrimitiveTypeKind::I16:
            case PrimitiveTypeKind::U16:
                _valueOfLastType = llvm::Type::getInt16Ty(_context);
                break;
            case PrimitiveTypeKind::I32:
            case PrimitiveTypeKind::U32:
                _valueOfLastType = llvm::Type::getInt32Ty(_context);
                break;
            case PrimitiveTypeKind::I64:
            case PrimitiveTypeKind::U64:
                _valueOfLastType = llvm::Type::getInt64Ty(_context);
                break;
            case PrimitiveTypeKind::F32:
                _valueOfLastType = llvm::Type::getFloatTy(_context);
                break;
            case PrimitiveTypeKind::F64:
                _valueOfLastType = llvm::Type::getDoubleTy(_context);
                break;
            default:
                // TODO: how to handle the other types
                assert(false);
        }
    }

    void LLVMTypeEmitVisitor::visitPointerType(PointerType *type) {
        auto prevOuterType = _outerType;
        _outerType = TypeKind::POINTER;
        auto innerType = getLLVMType(type->pointsTo());
        _outerType = prevOuterType;
        if (type->isArray()) {
            _valueOfLastType = llvm::ArrayType::get(innerType, type->size());
        } else {
            _valueOfLastType = llvm::PointerType::get(innerType, 0);
        }
    }

    void LLVMTypeEmitVisitor::visitCustomType(CustomType *type) {
        // TODO: how to handle the other types
        auto prevOuterType = _outerType;
        _outerType = TypeKind::CUSTOM;
        (void) type;
        _outerType = prevOuterType;
        assert(false);
    }
}