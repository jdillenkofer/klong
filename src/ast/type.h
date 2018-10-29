#pragma once

#include "lexer/token.h"
#include "common/source_range.h"
#include "visitor.h"

#include <vector>
#include <memory>

namespace klong {
    enum class TypeKind {
        FUNCTION,
        PRIMITIVE,
        POINTER,
        SIMPLE
    };

    enum class PrimitiveTypeKind {
        VOID,
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

        virtual void accept(TypeVisitor* visitor) = 0;

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

        static bool isBoolean(Type* type);
        static bool isFloat(Type* type);
        static bool isInteger(Type* type);
        static bool isPointer(Type* type);


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
                    std::make_shared<PrimitiveType>(PrimitiveTypeKind::VOID));
            }
        }

        void accept(TypeVisitor* visitor) {
            visitor->visitFunctionType(this);
        }

        std::vector<Type*> paramTypes() const {
            std::vector<Type*> paramTypes;
            for (auto& paramType : _paramTypes) {
                paramTypes.push_back(paramType.get());
            }
            return paramTypes;
        }

        Type* returnType() const {
            return _returnType.get();
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

                return matchesSignature(otherFunctionType->paramTypes());
            }
            return false;
        }

        Type* clone() const {
            return new FunctionType(SourceRange(), std::vector<TypePtr>(this->_paramTypes),
                    std::shared_ptr<Type>(this->_returnType->clone()));
        }

        bool matchesSignature(const std::vector<Type*>& callSignature) const {
            if (this->_paramTypes.size() != callSignature.size()) {
                return false;
            }
            for (size_t i = 0; i < this->_paramTypes.size(); i++) {
                if (!this->_paramTypes[i]->isEqual(callSignature[i])) {
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
        PrimitiveType(PrimitiveTypeKind type):
            Type(TypeKind::PRIMITIVE, SourceRange()), _type(type) {
        }

        PrimitiveType(SourceRange sourceRange, PrimitiveTypeKind type):
            Type(TypeKind::PRIMITIVE, sourceRange), _type(type) {
        }

        void accept(TypeVisitor* visitor) {
            visitor->visitPrimitiveType(this);
        }

        PrimitiveTypeKind type() const {
            return _type;
        }

        bool isEqual(const Type* other) const {
            if (other && other->kind() == TypeKind::PRIMITIVE) {
                auto otherPrimitiveType = dynamic_cast<const PrimitiveType*>(other);
                return this->type() == otherPrimitiveType->type();
            }
            return false;
        }

        bool isVoid() const {
            return type() == PrimitiveTypeKind::VOID;
        }

        bool isBoolean() const {
            return type() == PrimitiveTypeKind::BOOL;
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

    class PointerType : public Type {
    public:
        PointerType(SourceRange sourceRange, TypePtr type):
                Type(TypeKind::POINTER, sourceRange), _pointsTo(std::move(type)) {
        }

        PointerType(TypePtr type):
            Type(TypeKind::POINTER, SourceRange()), _pointsTo(std::move(type)) {

        }

        void accept(TypeVisitor* visitor) {
            visitor->visitPointerType(this);
        }

        Type* pointsTo() const {
            return _pointsTo.get();
        }

        bool isArray() const {
            return _isArray;
        }

        void isArray(bool val) {
            _isArray = val;
        }

        uint64_t size() const {
            return _size;
        }

        void size(uint64_t val) {
            _size = val;
        }

        bool isEqual(const Type* other) const {
            if (other->kind() == TypeKind::POINTER) {
                auto otherPointerType = dynamic_cast<const PointerType*>(other);
                auto innerTypesEqual = this->pointsTo()->isEqual(otherPointerType->pointsTo());
                if (this->isArray() && otherPointerType->isArray() && this->size() != otherPointerType->size()) {
                    return false;
                }
                return innerTypesEqual;
            }
            return false;
        }

        Type* clone() const {
            return new PointerType(SourceRange(), this->_pointsTo);
        }

    private:
        bool _isArray = false;
        uint64_t _size = 0;
        TypePtr _pointsTo;
    };

    class SimpleType : public Type {
    public:
        SimpleType(SourceRange sourceRange, std::string name):
            Type(TypeKind::SIMPLE, sourceRange),
            _name(std::move(name)) {
        }

        void accept(TypeVisitor* visitor) {
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