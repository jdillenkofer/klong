#pragma once

#include "../lexer/token.h"
#include "visitor.h"

#include <vector>
#include <memory>

namespace klong {
    class Type {
        public:
            virtual ~Type() = default;
            virtual void accept(Visitor* visitor) = 0;
            bool operator==(const Type& type) const {
                return this->isEqual(&type);
            }
            virtual bool isEqual(const Type* other) const = 0;
    };

    using TypePtr = std::shared_ptr<Type>;

    class FunctionType : public Type {
        public:
            FunctionType(std::vector<TypePtr>&& paramTypes, TypePtr returnType):
                _paramTypes(paramTypes), 
                _returnType(returnType) {
                if (returnType == nullptr) {
                    Token voidToken { nullptr, nullptr, TokenType::VOID };
                    _returnType = std::static_pointer_cast<Type>(
                        std::make_shared<BuiltInType>(voidToken));
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
                auto otherFunctionType = dynamic_cast<const FunctionType*>(other);
                if (otherFunctionType != nullptr) {
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

    class BuiltInType : public Type {
        public:
            BuiltInType(Token token):
                _token(token) {

            }

            void accept(Visitor* visitor) {
                visitor->visitBuiltInType(this);
            }

            Token token() const {
                return _token;
            }

            bool isEqual(const Type* other) const {
                auto otherBuiltInType = dynamic_cast<const BuiltInType*>(other);
                if (otherBuiltInType != nullptr) {
                    return this->token().type == otherBuiltInType->token().type;                    
                }
                return false;
            }

        private:
            Token _token;
    };

    class UserDefinedType : public Type {
        public:
            UserDefinedType(Token token):
                _token(token) {

            }

            void accept(Visitor* visitor) {
                visitor->visitUserDefinedType(this);
            }

            Token token() const {
                return _token;
            }

            bool isEqual(const Type* other) const {
                // TODO: rework this
                // maybe we need a symbol table here!?
                // how to support typedefs?
                auto otherUserDefinedType = dynamic_cast<const UserDefinedType*>(other);
                if (otherUserDefinedType != nullptr) {
                    return this->token().value == otherUserDefinedType->token().value;
                }
                return false;
            }

        private:
            Token _token;
    };
}