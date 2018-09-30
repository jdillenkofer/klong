#pragma once

#include "visitor.h"
#include "../common/source_range.h"
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
            _type = std::move(type);
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
        Assign(SourceRange sourceRange, std::shared_ptr<Variable> target, ExprPtr value):
            Expr(ExprKind::ASSIGN, sourceRange),
            _target(std::move(target)), _value(std::move(value)) {
        }

        void accept(Visitor* visitor) {
            visitor->visitAssignExpr(this);
        }

        std::shared_ptr<Variable> target() const {
            return _target;
        }

        ExprPtr value() const {
            return _value;
        }

    private:
        std::shared_ptr<Variable> _target;
        ExprPtr _value;
    };

    enum class BinaryOperation {
        PLUS,
        MINUS,
        MULTIPLICATION,
        DIVISION,
        MODULO,
        LSL,
        LSR,
        ASR,
        GREATER_THAN,
        GREATER_EQUAL,
        LESS_THAN,
        LESS_EQUAL,
        EQUALITY,
        INEQUALITY,
        AND,
        XOR,
        OR
    };
    
    class Binary : public Expr {
    public:
        Binary(SourceRange sourceRange, ExprPtr left, BinaryOperation op, ExprPtr right):
            Expr(ExprKind::BINARY, sourceRange),
            _left(std::move(left)), _op(op), _right(std::move(right)) {
        }

        void accept(Visitor* visitor) {
            visitor->visitBinaryExpr(this);
        }

        ExprPtr left() const {
            return _left;
        }

        BinaryOperation op() const {
            return _op;
        }

        ExprPtr right() const {
            return _right;
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
            _callee(std::move(callee)), _args(args) {
        }

        void accept(Visitor* visitor) {
            visitor->visitCallExpr(this);
        }

        ExprPtr callee() const {
            return _callee;
        }

        std::vector<ExprPtr> args() const {
            return _args;
        }

    private:
        ExprPtr _callee;
        std::vector<ExprPtr> _args;
    };
    
    class Grouping : public Expr {
    public:
        Grouping(SourceRange sourceRange, ExprPtr expr):
            Expr(ExprKind::GROUPING, sourceRange),
            _expr(std::move(expr)) {
        }

        void accept(Visitor* visitor) {
            visitor->visitGroupingExpr(this);
        }

        ExprPtr expression() const {
            return _expr;
        }

    private:
        ExprPtr _expr;
    };
    
    class Literal : public Expr {
    public:
        Literal(SourceRange sourceRange, PrimitiveTypeKind literalType):
            Expr(ExprKind::LITERAL, sourceRange),
            _literalType(literalType) {
        }

        Literal(PrimitiveTypeKind literalType):
            Expr(ExprKind::LITERAL, SourceRange()),
            _literalType(literalType) {
        }

        void accept(Visitor* visitor) = 0;

        PrimitiveTypeKind literalType() const {
            return _literalType;
        }

    private:
        PrimitiveTypeKind _literalType;
    };

    class NumberLiteral: public Literal {
    public:
        NumberLiteral(SourceRange sourceRange, double value):
            Literal(sourceRange, PrimitiveTypeKind::F64) {
            _value.f = value;
        }

        NumberLiteral(SourceRange sourceRange, int64_t value):
            Literal(sourceRange, PrimitiveTypeKind::I64) {
            _value.i = value;
        }

        NumberLiteral(SourceRange sourceRange, uint64_t value):
            Literal(sourceRange, PrimitiveTypeKind::U64) {
            _value.u = value;
        }

        double f64() const {
            return _value.f;
        }

        int64_t i64() const {
            return _value.i;
        };

        uint64_t u64() const {
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
    };

    class BoolLiteral: public Literal {
    public:
        BoolLiteral(SourceRange sourceRange, bool value):
            Literal(sourceRange, PrimitiveTypeKind::BOOL), _value(value) {
        }

        BoolLiteral(bool value):
            Literal(SourceRange(), PrimitiveTypeKind::BOOL), _value(value) {
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
            Literal(sourceRange, PrimitiveTypeKind::STRING), _value(std::move(value)) {
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
            Literal(sourceRange, PrimitiveTypeKind::I8), _value(value) {
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
            _left(std::move(left)), _op(op), _right(std::move(right)) {
        }

        void accept(Visitor* visitor) {
            visitor->visitLogicalExpr(this);
        }

        ExprPtr left() const {
            return _left;
        }

        ExprPtr right() const {
            return _right;
        }

        LogicalOperation op() const {
            return _op;
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
            _op(op), _right(std::move(right)) {
        }

        void accept(Visitor* visitor) {
            visitor->visitUnaryExpr(this);
        }

        UnaryOperation op() const {
            return _op;
        }

        ExprPtr right() const {
            return _right;
        }

    private:
        UnaryOperation _op;
        ExprPtr _right;
    };

    class Stmt;

    class Variable : public Expr {
    public:
        Variable(SourceRange sourceRange, std::string name):
            Expr(ExprKind::VARIABLE, sourceRange),
            _name(std::move(name)) {
        }

        void accept(Visitor* visitor) {
            visitor->visitVariableExpr(this);
        }

        const std::string& name() const {
            return _name;
        }

        void resolvesTo(Stmt* resolvesTo) {
            _resolvesTo = resolvesTo;
        }

        Stmt* resolvesTo() const {
            return _resolvesTo;
        }

    private:
        std::string _name;
        Stmt* _resolvesTo = nullptr;
    };
}