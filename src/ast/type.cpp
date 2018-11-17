#include "type.h"

namespace klong {
    bool Type::isBoolean(Type* type) {
        if (type && type->kind() == TypeKind::PRIMITIVE) {
            auto primitiveType = dynamic_cast<PrimitiveType*>(type);
            return primitiveType->isBoolean();
        }
        return false;
    }

    bool Type::isFloat(Type* type) {
        if (type && type->kind() == TypeKind::PRIMITIVE) {
            auto primitiveType = dynamic_cast<PrimitiveType*>(type);
            return primitiveType->isFloat();
        }
        return false;
    }

    bool Type::isInteger(Type* type) {
        if (type && type->kind() == TypeKind::PRIMITIVE) {
            auto primitiveType = dynamic_cast<PrimitiveType*>(type);
            return primitiveType->isInteger();
        }
        return false;
    }

    bool Type::isPointer(Type* type) {
        if (type->kind() == TypeKind::POINTER) {
            auto pointerType = dynamic_cast<PointerType*>(type);
            return pointerType != nullptr;
        }
        return false;
    }

    bool Type::isVoid(Type* type) {
        if (type && type->kind() == TypeKind::PRIMITIVE) {
            auto primitiveType = dynamic_cast<PrimitiveType*>(type);
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
}