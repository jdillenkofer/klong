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
        if (Type::isPointer(otherType)) {
            auto otherPointerType = static_cast<PointerType*>(otherType);
            if (Type::isVoid(otherPointerType->pointsTo())) {
                if (Type::isPointer(exprToModifiy->type())) {
                    exprToModifiy->castToType(std::shared_ptr<Type>(otherType->clone()));
                    return true;
                }
            }
        }
        return false;
    }

    bool FunctionType::isEqual(const Type* other) const {
        if (other->kind() == TypeKind::FUNCTION) {
            auto otherFunctionType = static_cast<const FunctionType*>(other);
            if (this->_paramTypes.size() != otherFunctionType->_paramTypes.size()) {
                return false;
            }

            if (this->_isVariadic != otherFunctionType->_isVariadic) {
                return false;
            }

            if (!this->_returnType->isEqual(otherFunctionType->_returnType.get())) {
                return false;
            }

            return matchesSignature(otherFunctionType->paramTypes());
        }
        return false;
    }

    Type* FunctionType::clone() const {
        return new FunctionType(SourceRange(), std::vector<TypePtr>(this->_paramTypes),
            std::shared_ptr<Type>(this->_returnType->clone()), _isVariadic);
    }

    bool FunctionType::matchesSignature(const std::vector<Type*>& callSignature) const {
        if (this->_paramTypes.size() != callSignature.size()) {
            for (size_t i = 0; i < this->_paramTypes.size(); i++) {
                if (!this->_paramTypes[i]->isEqual(callSignature[i])) {
                    return false;
                }
            }
        }
        return true;
    }

    bool FunctionType::matchesSignature(std::vector<Expr*>& arguments) const {
        auto& expectedTypes = _paramTypes;
        if (expectedTypes.size() != arguments.size()) {
            return false;
        }
        for (size_t i = 0; i < expectedTypes.size(); i++) {
            auto argument = arguments[i];
            auto argumentType = argument->type();
            if (!expectedTypes[i]->isEqual(argumentType)) {
                if (Type::isVoidPtrCast(argument, expectedTypes[i].get())) {
                    continue;
                }
                return false;
            }
        }
        return true;
    }
}