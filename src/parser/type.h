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

    enum class PrimitiveTypeKind {
        VOID,
        STRING,
        BOOL,
        I8,
        I16,
        I32,
        I64,
        U8,
        U16,
        U32,
        U64,
        F32,
        F64
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

            virtual Type* clone() const = 0;

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
                    _returnType = std::static_pointer_cast<Type>(
                        std::make_shared<PrimitiveType>(SourceRange(), PrimitiveTypeKind::VOID));
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

            Type* clone() const {
                return new FunctionType(SourceRange(), this->paramTypes(), this->returnType());
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
            PrimitiveType(SourceRange sourceRange, PrimitiveTypeKind type):
                Type(TypeKind::PRIMITIVE, sourceRange), _type(type) {

            }

            void accept(Visitor* visitor) {
                visitor->visitPrimitiveType(this);
            }

            PrimitiveTypeKind type() const {
                return _type;
            }

            bool isEqual(const Type* other) const {
                if (other->kind() == TypeKind::PRIMITIVE) {
                    auto otherPrimitiveType = dynamic_cast<const PrimitiveType*>(other);
                    return this->type() == otherPrimitiveType->type();
                }
                return false;
            }

            bool isBoolean() const {
                return type() == PrimitiveTypeKind::BOOL;
            }

            bool isString() const {
                switch(type()) {
                    case PrimitiveTypeKind::STRING:
                        return true;
                    default:
                        return false;
                }
            }

            bool isInteger() const {
                switch(type()) {
                    case PrimitiveTypeKind::U8:
                    case PrimitiveTypeKind::U16:
                    case PrimitiveTypeKind::U32:
                    case PrimitiveTypeKind::U64:
                    case PrimitiveTypeKind::I8:
                    case PrimitiveTypeKind::I16:
                    case PrimitiveTypeKind::I32:
                    case PrimitiveTypeKind::I64:
                        return true;
                    default:
                        return false;
                }
            }

            bool isSigned() const {
                switch(type()) {
                    case PrimitiveTypeKind::I8:
                    case PrimitiveTypeKind::I16:
                    case PrimitiveTypeKind::I32:
                    case PrimitiveTypeKind::I64:
                        return true;
                    case PrimitiveTypeKind::U8:
                    case PrimitiveTypeKind::U16:
                    case PrimitiveTypeKind::U32:
                    case PrimitiveTypeKind::U64:
                    default:
                        return false;
                }
            }

            bool isFloat() const {
                switch(type()) {
                    case PrimitiveTypeKind::F32:
                    case PrimitiveTypeKind::F64:
                        return true;
                    default:
                        return false;
                }
            }

            Type* clone() const {
                return new PrimitiveType(SourceRange(), this->type());
            }

        private:
            PrimitiveTypeKind _type;
    };

    class SimpleType : public Type {
        public:
            SimpleType(SourceRange sourceRange, std::string name):
                Type(TypeKind::SIMPLE, sourceRange),
                _name(std::move(name)) {

            }

            void accept(Visitor* visitor) {
                visitor->visitSimpleType(this);
            }

            const std::string& name() const {
                return _name;
            }

            bool isEqual(const Type* other) const {
                // TODO: rework this
                // maybe we need a symbol table here!?
                // how to support typedefs?
                if (other->kind() == TypeKind::SIMPLE) {
                    auto otherSimpleType = dynamic_cast<const SimpleType*>(other);
                    return this->name() == otherSimpleType->name();
                }
                return false;
            }

            Type* clone() const {
                return new SimpleType(SourceRange(), this->name());
            }

        private:
            std::string _name;
    };
}