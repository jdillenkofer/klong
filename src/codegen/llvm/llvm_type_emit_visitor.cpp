#include <llvm/IR/DerivedTypes.h>
#include "ast/stmt.h"
#include "llvm_type_emit_visitor.h"

namespace klong {

    llvm::Type* LLVMTypeEmitVisitor::getLLVMType(Type* type) {
        type->accept(this);
        return _valueOfLastType;
    }

    void LLVMTypeEmitVisitor::visitFunctionType(FunctionType* type) {
        std::vector<llvm::Type*> paramTypes;

        _outerTypes.push_back(TypeKind::FUNCTION);

        for (auto& paramType : type->paramTypes()) {
            paramTypes.push_back(getLLVMType(paramType));
        }

		llvm::Type* returnType = getLLVMType(type->returnType());

        _outerTypes.pop_back();

        _valueOfLastType = llvm::FunctionType::get(returnType, paramTypes, type->isVariadic());
    }

    void LLVMTypeEmitVisitor::visitPrimitiveType(PrimitiveType* type) {
        switch (type->type()) {
            case PrimitiveTypeKind::VOID:
            {
                if (!_outerTypes.empty() && _outerTypes.back() == TypeKind::POINTER) {
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
                assert(false);
        }
    }

    void LLVMTypeEmitVisitor::visitPointerType(PointerType* type) {
        _outerTypes.push_back(TypeKind::POINTER);
        auto innerType = getLLVMType(type->pointsTo());
		_outerTypes.pop_back();
        if (type->isArray()) {
            _valueOfLastType = llvm::ArrayType::get(innerType, type->size());
        } else {
            _valueOfLastType = llvm::PointerType::get(innerType, 0);
        }
    }

    void LLVMTypeEmitVisitor::visitCustomType(CustomType* type) {
        auto it = _customTypeCache.find(type->name());
        if (it != _customTypeCache.end()) {
            _valueOfLastType = (*it).second;
            return;
        }

		// TODO: emit debug information for custom types

        _outerTypes.push_back(TypeKind::CUSTOM);
        auto typeDeclaration = type->resolvesTo();
        switch(typeDeclaration->typeDeclarationKind()) {
            case TypeDeclarationKind::STRUCT:
            {
                auto structDeclaration = dynamic_cast<StructDeclaration*>(typeDeclaration);
                assert(structDeclaration);

                if (_outerTypes.size() > 1u) {
                    auto previousOuterType = _outerTypes[_outerTypes.size() - 2];
                    if (previousOuterType == TypeKind::POINTER
                        && structDeclaration->isSelfReferential()) {
                        // this emits an opaque struct type
                        _valueOfLastType = llvm::StructType::create(_context, type->name());
                        return;
                    }
                }

                std::vector<llvm::Type*> members;
                for (auto& value : structDeclaration->members()) {
                    members.push_back(getLLVMType(value->type()));
                }

                _valueOfLastType = llvm::StructType::create(_context, members, type->name());
                _customTypeCache[type->name()] = _valueOfLastType;
                break;
            }
            case TypeDeclarationKind::UNION:
            {
                auto unionDeclaration = dynamic_cast<UnionDeclaration*>(typeDeclaration);
                assert(unionDeclaration);

                if (_outerTypes.size() > 1u) {
                    auto previousOuterType = _outerTypes[_outerTypes.size() - 2];
                    if (previousOuterType == TypeKind::POINTER
                        && unionDeclaration->isSelfReferential()) {
                        // this emits an opaque union type
                        _valueOfLastType = llvm::StructType::create(_context, type->name());
                        return;
                    }
                }

                llvm::Type* biggestLLVMType = nullptr;
                uint64_t biggestSizeInBits = 0;
                for (auto& value : unionDeclaration->members()) {
                    auto currentLLVMType = getLLVMType(value->type());
                    auto currentTypeSize = _dataLayout.getTypeSizeInBits(currentLLVMType);
                    if (currentTypeSize > biggestSizeInBits) {
                        biggestSizeInBits = currentTypeSize;
                        biggestLLVMType = currentLLVMType;
                    }
                }

                // only add the biggest llvm type to the union
                std::vector<llvm::Type*> members;
				if (biggestLLVMType != nullptr) {
					members.push_back(biggestLLVMType);
				}

                _valueOfLastType = llvm::StructType::create(_context, members, type->name());
                _customTypeCache[type->name()] = _valueOfLastType;
                break;
            }
			case TypeDeclarationKind::ENUM:
			{
				_valueOfLastType = llvm::Type::getInt32Ty(_context);
				break;
			}
            default:
                assert(false);
                break;
        }
        _outerTypes.pop_back();
    }
}