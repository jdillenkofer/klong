#pragma once

#include "visitor.h"
#include "../lexer/token.h"
#include "type.h"

#include <vector>
#include <memory>

namespace klong {

    enum class ExprKind {
        ASSIGN,
        BINARY,
        CALL,
        GROUPING,
        LITERAL,
        LOGICAL,
        UNARY,
        VARIABLE
    };
    
    class Expr {
        public:
            Expr(ExprKind kind, SourceRange sourceRange): _kind(kind),
                _sourceRange(sourceRange) {

            }

            virtual ~Expr() = default;

            virtual void accept(Visitor* visitor) = 0;

            ExprKind kind() const {
                return _kind;
            }

            void type(TypePtr type) {
                _type = type;
            }

            TypePtr type() const {
                return _type;
            }

            SourceRange sourceRange() const {
                return _sourceRange;
            }

        private:
            ExprKind _kind;
            TypePtr _type = nullptr;
            SourceRange _sourceRange;
    };

    using ExprPtr = std::shared_ptr<Expr>;

    class Assign : public Expr {
        public:
            Assign(SourceRange sourceRange, const std::string& target, ExprPtr value):
                Expr(ExprKind::ASSIGN, sourceRange),
                _target(target), _value(value) {

            }

            void accept(Visitor* visitor) {
                visitor->visitAssignExpr(this);
            }

            const std::string& target() const {
                return _target;
            }

        private:
            std::string _target;
            ExprPtr _value;
    };

    enum class BinaryOperation {
        PLUS,
        MINUS,
        MULTIPLICATION,
        DIVISION,
        EQUALITY,
        INEQUALITY,
        GREATER_THAN,
        GREATER_EQUAL,
        LESS_THAN,
        LESS_EQUAL
    };
    
    class Binary : public Expr {
        public:
            Binary(SourceRange sourceRange, ExprPtr left, BinaryOperation op, ExprPtr right):
                Expr(ExprKind::BINARY, sourceRange),
                _left(left), _op(op), _right(right) {

            }

            void accept(Visitor* visitor) {
                visitor->visitBinaryExpr(this);
            }

        private:
            ExprPtr _left;
            BinaryOperation _op;
            ExprPtr _right;
    };
    
    class Call : public Expr {
        public:
            Call(SourceRange sourceRange, ExprPtr callee, std::vector<ExprPtr>&& args):
                Expr(ExprKind::CALL, sourceRange),
                _callee(callee), _args(args) {
            }

            void accept(Visitor* visitor) {
                visitor->visitCallExpr(this);
            }

        private:
            ExprPtr _callee;
            std::vector<ExprPtr> _args;
    };
    
    class Grouping : public Expr {
        public:
            Grouping(SourceRange sourceRange, ExprPtr expr):
                Expr(ExprKind::GROUPING, sourceRange),
                _expr(expr) {

            }

            void accept(Visitor* visitor) {
                visitor->visitGroupingExpr(this);
            }

        private:
            ExprPtr _expr;
    };

    enum class LiteralType {
        NUMBER,
        BOOL,
        STRING,
        CHAR
    };
    
    class Literal : public Expr {
        public:
            Literal(SourceRange sourceRange, LiteralType literalType):
                Expr(ExprKind::LITERAL, sourceRange),
                _literalType(literalType) {

            }

            Literal(LiteralType literalType):
                Expr(ExprKind::LITERAL, SourceRange {nullptr, nullptr}),
                _literalType(literalType) {

            }

            void accept(Visitor* visitor) = 0;

            LiteralType literalType() const {
                return _literalType;
            }

        private:
            LiteralType _literalType;
    };

    class NumberLiteral: public Literal {
        public:
            NumberLiteral(SourceRange sourceRange, double value):
                    Literal(sourceRange, LiteralType::NUMBER), _type(NumberType::F64) {
                _value.f = value;
            }

            NumberLiteral(SourceRange sourceRange, int64_t value):
                    Literal(sourceRange, LiteralType::NUMBER), _type(NumberType::I64) {
                _value.i = value;
            }

            NumberLiteral(SourceRange sourceRange, uint64_t value):
                Literal(sourceRange, LiteralType::NUMBER), _type(NumberType::U64) {
                _value.u = value;
            }

            NumberType numberType() const {
                return _type;
            }

            double f64() const {
                return _value.f;
            }

            uint64_t i64() const {
                return _value.i;
            };

            int64_t u64() const {
                return _value.u;
            }

            void accept(Visitor* visitor) {
                visitor->visitNumberLiteral(this);
            };
    private:
            union {
                double f;
                int64_t i;
                uint64_t u;
            } _value;
            NumberType _type;
    };

    class BoolLiteral: public Literal {
        public:
            BoolLiteral(SourceRange sourceRange, bool value):
                Literal(sourceRange, LiteralType::BOOL), _value(value) {
            }

            BoolLiteral(bool value):
                    Literal(SourceRange { nullptr, nullptr }, LiteralType::BOOL), _value(value) {
            }

            bool value() const {
                return _value;
            }

            void accept(Visitor* visitor) {
                visitor->visitBoolLiteral(this);
            };
        private:
            bool _value;
    };

    class StringLiteral: public Literal {
        public:
            StringLiteral(SourceRange sourceRange, std::string value):
                Literal(sourceRange, LiteralType::STRING), _value(std::move(value)) {
            }

            const std::string& value() const {
                return _value;
            }

            void accept(Visitor* visitor) {
                visitor->visitStringLiteral(this);
            };
        private:
            std::string _value;
    };

    class CharacterLiteral: public Literal {
        public:
            CharacterLiteral(SourceRange sourceRange, char value):
                Literal(sourceRange, LiteralType::CHAR), _value(value) {
            }

            char value() const {
                return _value;
            }

            void accept(Visitor* visitor) {
                visitor->visitCharacterLiteral(this);
            };
        private:
            char _value;
    };

    enum class LogicalOperation {
        OR,
        AND
    };
    
    class Logical : public Expr {
        public:
            Logical(SourceRange sourceRange, ExprPtr left, LogicalOperation op, ExprPtr right):
                Expr(ExprKind::LOGICAL, sourceRange),
                _left(left), _op(op), _right(right) {

            }

            void accept(Visitor* visitor) {
                visitor->visitLogicalExpr(this);
            }

        private:
            ExprPtr _left;
            LogicalOperation _op;
            ExprPtr _right;
    };

    enum class UnaryOperation {
        NOT,
        MINUS
    };

    class Unary : public Expr {
        public:
            Unary(SourceRange sourceRange, UnaryOperation op, ExprPtr right):
                Expr(ExprKind::UNARY, sourceRange),
                _op(op), _right(right) {

            }

            void accept(Visitor* visitor) {
                visitor->visitUnaryExpr(this);
            }

        private:
            UnaryOperation _op;
            ExprPtr _right;
    };

    class Variable : public Expr {
        public:
            Variable(SourceRange sourceRange, const std::string& name):
                Expr(ExprKind::VARIABLE, sourceRange),
                _name(name) {

            }

            void accept(Visitor* visitor) {
                visitor->visitVariableExpr(this);
            }

            const std::string& name() const {
                return _name;
            }

        private:
            std::string _name;
    };
}