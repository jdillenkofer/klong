#include <llvm/IR/DerivedTypes.h>
#include "ast/stmt.h"
#include "llvm_debug_type_emit_visitor.h"

namespace klong {

	llvm::DIType* LLVMDebugTypeEmitVisitor::getLLVMDebugType(Type* type) {
		assert(_debugInfoBuilder != nullptr);
		type->accept(this);
		return _valueOfLastDebugType;
	}

	void LLVMDebugTypeEmitVisitor::visitFunctionType(FunctionType* type) {
		assert(_debugInfoBuilder != nullptr);
		llvm::SmallVector<llvm::Metadata*, 8> paramDebugTypes;

		_outerTypes.push(TypeKind::FUNCTION);

		llvm::DIType* returnDebugType = getLLVMDebugType(type->returnType());
		paramDebugTypes.push_back(returnDebugType);

		for (auto& paramType : type->paramTypes()) {
			paramDebugTypes.push_back(getLLVMDebugType(paramType));
		}

		_outerTypes.pop();
		auto typeArray = _debugInfoBuilder->getOrCreateTypeArray(paramDebugTypes);
		_valueOfLastDebugType = _debugInfoBuilder->createSubroutineType(typeArray);
	}

	void LLVMDebugTypeEmitVisitor::visitPrimitiveType(PrimitiveType* type) {
		assert(_debugInfoBuilder != nullptr);
		switch (type->type()) {
			case PrimitiveTypeKind::VOID:
				if (!_outerTypes.empty() && _outerTypes.back() == TypeKind::FUNCTION) {
					_valueOfLastDebugType = nullptr;
					break;
				}
				_valueOfLastDebugType = _debugInfoBuilder->createUnspecifiedType("void");
				break;
			case PrimitiveTypeKind::BOOL:
				_valueOfLastDebugType = _debugInfoBuilder->createBasicType("bool", 8, llvm::dwarf::DW_ATE_boolean);
				break;
			case PrimitiveTypeKind::I8:
				if (!_outerTypes.empty() && _outerTypes.back() == TypeKind::POINTER) {
					_valueOfLastDebugType = _debugInfoBuilder->createBasicType("i8", 8, llvm::dwarf::DW_ATE_signed_char);
					break;
				}
				_valueOfLastDebugType = _debugInfoBuilder->createBasicType("i8", 8, llvm::dwarf::DW_ATE_signed);
				break;
			case PrimitiveTypeKind::U8:
				_valueOfLastDebugType = _debugInfoBuilder->createBasicType("u8", 8, llvm::dwarf::DW_ATE_unsigned);
				break;
			case PrimitiveTypeKind::I16:
				_valueOfLastDebugType = _debugInfoBuilder->createBasicType("i16", 16, llvm::dwarf::DW_ATE_signed);
				break;
			case PrimitiveTypeKind::U16:
				_valueOfLastDebugType = _debugInfoBuilder->createBasicType("u16", 16, llvm::dwarf::DW_ATE_unsigned);
				break;
			case PrimitiveTypeKind::I32:
				_valueOfLastDebugType = _debugInfoBuilder->createBasicType("i32", 32, llvm::dwarf::DW_ATE_signed);
				break;
			case PrimitiveTypeKind::U32:
				_valueOfLastDebugType = _debugInfoBuilder->createBasicType("u32", 32, llvm::dwarf::DW_ATE_unsigned);
				break;
			case PrimitiveTypeKind::I64:
				_valueOfLastDebugType = _debugInfoBuilder->createBasicType("i64", 64, llvm::dwarf::DW_ATE_signed);
				break;
			case PrimitiveTypeKind::U64:
				_valueOfLastDebugType = _debugInfoBuilder->createBasicType("u64", 64, llvm::dwarf::DW_ATE_unsigned);
				break;
			case PrimitiveTypeKind::F32:
				_valueOfLastDebugType = _debugInfoBuilder->createBasicType("f32", 32, llvm::dwarf::DW_ATE_float);
				break;
			case PrimitiveTypeKind::F64:
				_valueOfLastDebugType = _debugInfoBuilder->createBasicType("f64", 64, llvm::dwarf::DW_ATE_float);
				break;
			default:
				assert(false);
		}
	}

	void LLVMDebugTypeEmitVisitor::visitPointerType(PointerType* type) {
		assert(_debugInfoBuilder != nullptr);
		_outerTypes.push(TypeKind::POINTER);
		auto innerDebugType = getLLVMDebugType(type->pointsTo());
		_outerTypes.pop();
		_valueOfLastDebugType = _debugInfoBuilder->createPointerType(innerDebugType, _dataLayout.getPointerSizeInBits());
	}

	void LLVMDebugTypeEmitVisitor::visitCustomType(CustomType* type) {
		assert(_debugInfoBuilder != nullptr);
		
		// TODO: emit debug information for custom types
		// Note: STRUCT IS STILL VERY BUGGY!!!
		auto it = _customTypeCache.find(type->name());
		if (it != _customTypeCache.end()) {
			_valueOfLastDebugType = (*it).second;
			return;
		}

		auto debugFile = _debugScopeManager->getDebugFile();
		auto scope = _debugScopeManager->getDebugScope();
		auto line = type->sourceRange().start.line();

		_outerTypes.push(TypeKind::CUSTOM);
		auto typeDeclaration = type->resolvesTo();
		switch (typeDeclaration->typeDeclarationKind()) {
		case TypeDeclarationKind::STRUCT:
		{
			auto structDeclaration = static_cast<StructDeclaration*>(typeDeclaration);

			if (_outerTypes.size() > 1u) {
				auto previousOuterType = _outerTypes[_outerTypes.size() - 2];
				if (previousOuterType == TypeKind::POINTER
					&& structDeclaration->isSelfReferential()) {
					// this emits an opaque struct type
					_valueOfLastDebugType = _debugInfoBuilder->createForwardDecl(llvm::dwarf::DW_TAG_structure_type, type->name(), scope, debugFile, line);
					return;
				}
			}

			Array<llvm::Metadata*> elements;
			uint64_t structSizeInBits = 0;
			for (auto& value : structDeclaration->members()) {
				auto actualDebugType = getLLVMDebugType(value->type());
				auto memberDebugType = _debugInfoBuilder->createMemberType(scope, value->name(), debugFile, value->sourceRange().start.line(), 
					actualDebugType->getSizeInBits(), actualDebugType->getAlignInBits(), actualDebugType->getOffsetInBits(), llvm::DINode::DIFlags::FlagZero, actualDebugType);
				structSizeInBits += actualDebugType->getSizeInBits();
				elements.push(memberDebugType);
			}

			llvm::DINodeArray members = _debugInfoBuilder->getOrCreateArray(llvm::ArrayRef(elements.data(), elements.size()));

			_valueOfLastDebugType = _debugInfoBuilder->createStructType(scope, type->name(), 
				debugFile, line, structSizeInBits, _dataLayout.getPointerPrefAlignment(), 
				llvm::DINode::DIFlags::FlagZero, nullptr, members);
			_customTypeCache[type->name()] = _valueOfLastDebugType;
			break;
		}
		case TypeDeclarationKind::UNION:
		{
			auto unionDeclaration = static_cast<UnionDeclaration*>(typeDeclaration);

			if (_outerTypes.size() > 1u) {
				auto previousOuterType = _outerTypes[_outerTypes.size() - 2];
				if (previousOuterType == TypeKind::POINTER
					&& unionDeclaration->isSelfReferential()) {
					// this emits an opaque union type
					// _valueOfLastDebugType = _debugInfoBuilder->createForwardDecl(llvm::dwarf::DW_TAG_union_type, type->name(),)
					return;
				}
			}

			Array<llvm::DIType*> members;
			for (auto& value : unionDeclaration->members()) {
				members.push(getLLVMDebugType(value->type()));
			}

			// _valueOfLastDebugType = _debugInfoBuilder->createUnionType(scope, type->name(), _debugFile, );
			_customTypeCache[type->name()] = _valueOfLastDebugType;
			break;
		}
		case TypeDeclarationKind::ENUM:
		{
			// _valueOfLastDebugType = _debugInfoBuilder->createEnumerationType(scope, type->name(), debugFile, line, );
			break;
		}
		default:
			assert(false);
			break;
		}
		_outerTypes.pop();
	}

	void LLVMDebugTypeEmitVisitor::setDebugInfoBuilder(llvm::DIBuilder* debugInfoBuilder) {
		_debugInfoBuilder = debugInfoBuilder;
	}
}