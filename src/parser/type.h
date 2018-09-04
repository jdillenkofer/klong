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
            std::vector<TypePtr> paramTypes() {
                return _paramTypes;
            }
            TypePtr returnType() {
                return _returnType;
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
            Token token() {
                return _token;
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
            Token token() {
                return _token;
            }
        private:
            Token _token;
    };
}