#pragma once

#include "../lexer/token.h"
#include "../common/source_range.h"
#include "visitor.h"

#include <vector>
#include <memory>

namespace klong {
    enum class TypeKind {
        FUNCTION,
        PRIMITIVE,
        SIMPLE
    };

    class Type {
        public:
            Type(TypeKind kind, SourceRange sourceRange):
                _kind(kind), _sourceRange(sourceRange) {

            }

            virtual ~Type() = default;

            virtual void accept(Visitor* visitor) = 0;

            TypeKind kind() const {
                return _kind;
            }

            SourceRange sourceRange() const {
                return _sourceRange;
            }

            bool operator==(const Type& type) const {
                return this->isEqual(&type);
            }

            virtual bool isEqual(const Type* other) const = 0;

        private:
            TypeKind _kind;
            SourceRange _sourceRange;
    };

    using TypePtr = std::shared_ptr<Type>;

    class FunctionType : public Type {
        public:
            FunctionType(SourceRange sourceRange, std::vector<TypePtr>&& paramTypes, TypePtr returnType):
                Type(TypeKind::FUNCTION, sourceRange),
                _paramTypes(paramTypes),
                _returnType(returnType) {
                if (returnType == nullptr) {
                    Token voidToken { { nullptr, nullptr }, TokenType::VOID };
                    _returnType = std::static_pointer_cast<Type>(
                        std::make_shared<PrimitiveType>(SourceRange{ nullptr, nullptr }, voidToken));
                }
            }

            void accept(Visitor* visitor) {
                visitor->visitFunctionType(this);
            }

            std::vector<TypePtr> paramTypes() const {
                return _paramTypes;
            }

            const TypePtr returnType() const {
                return _returnType;
            }

            bool isEqual(const Type* other) const {
                if (other->kind() == TypeKind::FUNCTION) {
                    auto otherFunctionType = dynamic_cast<const FunctionType*>(other);
                    if (this->_paramTypes.size() != otherFunctionType->_paramTypes.size()) {
                        return false;
                    }

                    if (!this->_returnType->isEqual(otherFunctionType->_returnType.get())) {
                        return false;
                    }

                    return matchesSignature(otherFunctionType->_paramTypes);
                }
                return false;
            }

            bool matchesSignature(const std::vector<TypePtr>& callSignature) const {
                for (size_t i = 0; i < this->_paramTypes.size(); i++) {
                    if (!this->_paramTypes[i]->isEqual(callSignature[i].get())) {
                        return false;
                    }
                }
                return true;
            }

        private:
            std::vector<TypePtr> _paramTypes;
            TypePtr _returnType;
    };

    class PrimitiveType : public Type {
        public:
            PrimitiveType(SourceRange sourceRange, Token token):
                Type(TypeKind::PRIMITIVE, sourceRange),
                _token(token) {

            }

            void accept(Visitor* visitor) {
                visitor->visitPrimitiveType(this);
            }

            Token token() const {
                return _token;
            }

            bool isEqual(const Type* other) const {
                if (other->kind() == TypeKind::PRIMITIVE) {
                    auto otherPrimitiveType = dynamic_cast<const PrimitiveType*>(other);
                    return this->token().type == otherPrimitiveType->token().type;
                }
                return false;
            }

        private:
            Token _token;
    };

    class SimpleType : public Type {
        public:
            SimpleType(SourceRange sourceRange, Token token):
                Type(TypeKind::SIMPLE, sourceRange),
                _token(token) {

            }

            void accept(Visitor* visitor) {
                visitor->visitSimpleType(this);
            }

            Token token() const {
                return _token;
            }

            bool isEqual(const Type* other) const {
                // TODO: rework this
                // maybe we need a symbol table here!?
                // how to support typedefs?
                if (other->kind() == TypeKind::SIMPLE) {
                    auto otherSimpleType = dynamic_cast<const SimpleType*>(other);
                    return this->token().value == otherSimpleType->token().value;
                }
                return false;
            }

        private:
            Token _token;
    };
}