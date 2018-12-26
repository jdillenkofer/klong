#include <llvm/IR/DerivedTypes.h>
#include "ast/stmt.h"
#include "llvm_debug_type_emit_visitor.h"

namespace klong {

	llvm::DIType* LLVMDebugTypeEmitVisitor::getLLVMDebugType(Type* type) {
		type->accept(this);
		return _valueOfLastDebugType;
	}

	void LLVMDebugTypeEmitVisitor::visitFunctionType(FunctionType* type) {
		bool emitDebug = _debugInfoBuilder != nullptr;

		llvm::SmallVector<llvm::Metadata*, 8> paramDebugTypes;

		_outerTypes.push_back(TypeKind::FUNCTION);

		if (emitDebug) {
			llvm::DIType* returnDebugType = getLLVMDebugType(type->returnType());
			paramDebugTypes.push_back(returnDebugType);
		}

		for (auto& paramType : type->paramTypes()) {
			if (emitDebug) {
				paramDebugTypes.push_back(getLLVMDebugType(paramType));
			}
		}

		_outerTypes.pop_back();
		if (emitDebug) {
			auto typeArray = _debugInfoBuilder->getOrCreateTypeArray(paramDebugTypes);
			_valueOfLastDebugType = _debugInfoBuilder->createSubroutineType(typeArray);
		}
	}

	void LLVMDebugTypeEmitVisitor::visitPrimitiveType(PrimitiveType* type) {
		bool emitDebug = _debugInfoBuilder != nullptr;
		if (emitDebug) {
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
	}

	void LLVMDebugTypeEmitVisitor::visitPointerType(PointerType* type) {
		bool emitDebug = _debugInfoBuilder != nullptr;
		_outerTypes.push_back(TypeKind::POINTER);
		auto innerType = getLLVMDebugType(type->pointsTo());
		auto innerDebugType = _valueOfLastDebugType;
		_outerTypes.pop_back();
		if (emitDebug) {
			_valueOfLastDebugType = _debugInfoBuilder->createPointerType(innerDebugType, _dataLayout.getPointerSizeInBits());
		}
	}

	void LLVMDebugTypeEmitVisitor::visitCustomType(CustomType* type) {
		// TODO: implement custom debug types
	}

	void LLVMDebugTypeEmitVisitor::setDebugInfoBuilder(llvm::DIBuilder* debugInfoBuilder) {
		_debugInfoBuilder = debugInfoBuilder;
	}
}