#include "type.h"
#include "expr.h"

namespace klong {
    bool Type::isBoolean(Type* type) {
        if (type && type->kind() == TypeKind::PRIMITIVE) {
            auto primitiveType = static_cast<PrimitiveType*>(type);
            return primitiveType->isBoolean();
        }
        return false;
    }

    bool Type::isFloat(Type* type) {
        if (type && type->kind() == TypeKind::PRIMITIVE) {
            auto primitiveType = static_cast<PrimitiveType*>(type);
            return primitiveType->isFloat();
        }
        return false;
    }

    bool Type::isInteger(Type* type) {
        if (type && type->kind() == TypeKind::PRIMITIVE) {
            auto primitiveType = static_cast<PrimitiveType*>(type);
            return primitiveType->isInteger();
        }
        return false;
    }

    bool Type::isPointer(Type* type) {
        if (type->kind() == TypeKind::POINTER) {
            auto pointerType = static_cast<PointerType*>(type);
            return pointerType != nullptr;
        }
        return false;
    }

    bool Type::isVoid(Type* type) {
        if (type && type->kind() == TypeKind::PRIMITIVE) {
            auto primitiveType = static_cast<PrimitiveType*>(type);
            return primitiveType->isVoid();
        }
        return false;
    }

	bool Type::isCustom(Type* type) {
		if (type&& type->kind() == TypeKind::CUSTOM) {
			return true;
		}
		return false;
	}

    bool Type::isVoidPtrCast(Expr* exprToModifiy, Type* otherType) {
        if (Type::isPointer(exprToModifiy->type())) {
            auto exprPointerType = static_cast<PointerType*>(exprToModifiy->type());
            if (Type::isVoid(exprPointerType->pointsTo())) {
                if (Type::isPointer(otherType)) {
                    exprToModifiy->castToType(std::shared_ptr<Type>(otherType->clone()));
                    return true;
                }
            }
        }
        return false;
    }
}