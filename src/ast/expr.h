#pragma once

#include "visitor.h"
#include "common/source_range.h"
#include "type.h"

#include <vector>
#include <memory>

namespace klong {

    enum class ExprKind {
        ASSIGN,
        BINARY,
        CALL,
        GROUPING,
        SUBSCRIPT,
        MEMBER_ACCESS,
		ENUM_ACCESS,
		LITERAL,
        LOGICAL,
        UNARY,
        VARIABLE,
        SIZE_OF,
        CAST
    };
    
    class Expr {
    public:
        Expr(ExprKind kind, SourceRange sourceRange): _kind(kind),
            _sourceRange(std::move(sourceRange)) {
        }

        virtual ~Expr() = default;

        virtual void accept(ExprVisitor* visitor) = 0;

        ExprKind kind() const {
            return _kind;
        }

        void type(TypePtr type) {
            _type = std::move(type);
        }

        Type* type() const {
            return _type.get();
        }

        void castToType(TypePtr castType) {
            _castToType = std::move(castType);
        }

        Type* castToType() const {
            return _castToType.get();
        }

        SourceRange sourceRange() const {
            return _sourceRange;
        }

    private:
        ExprKind _kind;
        TypePtr _type = nullptr;
        TypePtr _castToType = nullptr;
        SourceRange _sourceRange;
    };

    using ExprPtr = std::shared_ptr<Expr>;

    class Assign : public Expr {
    public:
        Assign(SourceRange sourceRange, std::shared_ptr<Expr> target, ExprPtr value):
                Expr(ExprKind::ASSIGN, std::move(sourceRange)),
                _target(std::move(target)), _value(std::move(value)) {
        }

        void accept(ExprVisitor* visitor) override {
            visitor->visitAssignExpr(this);
        }

        Expr* target() const {
            return _target.get();
        }

        Expr* value() const {
            return _value.get();
        }

    private:
        std::shared_ptr<Expr> _target = nullptr;
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
            Expr(ExprKind::BINARY, std::move(sourceRange)),
            _left(std::move(left)), _op(op), _right(std::move(right)) {
        }

        void accept(ExprVisitor* visitor) override {
            visitor->visitBinaryExpr(this);
        }

        Expr* left() const {
            return _left.get();
        }

        BinaryOperation op() const {
            return _op;
        }

        Expr* right() const {
            return _right.get();
        }

    private:
        ExprPtr _left;
        BinaryOperation _op;
        ExprPtr _right;
    };
    
    class Call : public Expr {
    public:
        Call(SourceRange sourceRange, ExprPtr callee, std::vector<ExprPtr>&& args):
            Expr(ExprKind::CALL, std::move(sourceRange)),
            _callee(std::move(callee)), _args(args) {
        }

        void accept(ExprVisitor* visitor) override {
            visitor->visitCallExpr(this);
        }

        Expr* callee() const {
            return _callee.get();
        }

        std::vector<Expr*> args() const {
            std::vector<Expr*> args;
            for (auto& arg : _args) {
                args.push_back(arg.get());
            }
            return args;
        }

    private:
        ExprPtr _callee;
        std::vector<ExprPtr> _args;
    };
    
    class Grouping : public Expr {
    public:
        Grouping(SourceRange sourceRange, ExprPtr expr):
            Expr(ExprKind::GROUPING, std::move(sourceRange)),
            _expr(std::move(expr)) {
        }

        void accept(ExprVisitor* visitor) override {
            visitor->visitGroupingExpr(this);
        }

        Expr* expression() const {
            return _expr.get();
        }

    private:
        ExprPtr _expr;
    };
    
	class Subscript : public Expr {
	public:
		Subscript(SourceRange sourceRange, ExprPtr target, ExprPtr index): 
			Expr(ExprKind::SUBSCRIPT, std::move(sourceRange)),
			_target(std::move(target)),
			_index(std::move(index)) {
		}

		void accept(ExprVisitor* visitor) override {
			visitor->visitSubscriptExpr(this);
		}

		Expr* target() const {
			return _target.get();
		}

		Expr* index() const {
			return _index.get();
		}

	private:
		ExprPtr _target;
		ExprPtr _index;
	};

	class MemberAccess : public Expr {
	public:
	    MemberAccess(SourceRange sourceRange, ExprPtr target, std::string value):
	        Expr(ExprKind::MEMBER_ACCESS, std::move(sourceRange)),
	        _target(std::move(target)), _value(std::move(value)) {
	    }

	    void accept(ExprVisitor* visitor) override {
	        visitor->visitMemberAccessExpr(this);
	    }

        Expr* target() const {
            return _target.get();
        }

        const std::string& value() const {
	        return _value;
        }

	private:
	    ExprPtr _target;
	    std::string _value;
	};

	class EnumAccess : public Expr {
	public:
		EnumAccess(SourceRange sourceRange, std::shared_ptr<CustomType> target, std::string value) :
			Expr(ExprKind::ENUM_ACCESS, std::move(sourceRange)),
			_target(std::move(target)), 
			_value(std::move(value)) {
		}

		void accept(ExprVisitor* visitor) override {
			visitor->visitEnumAccessExpr(this);
		}

		CustomType* target() const {
			return _target.get();
		}

		const std::string& value() const {
			return _value;
		}
	private:
		std::shared_ptr<CustomType> _target;
		std::string _value;
	};

    class Literal : public Expr {
    public:
        Literal(SourceRange sourceRange, TypePtr literalType):
            Expr(ExprKind::LITERAL, std::move(sourceRange)),
            _literalType(std::move(literalType)) {
        }

        explicit Literal(TypePtr literalType):
            Expr(ExprKind::LITERAL, SourceRange()),
            _literalType(std::move(literalType)) {
        }

        explicit Literal(SourceRange sourceRange):
            Expr(ExprKind::LITERAL, std::move(sourceRange)) {
        }

        void accept(ExprVisitor* visitor) override = 0;

        Type* literalType() const {
            return _literalType.get();
        }

        void literalType(TypePtr type) {
            _literalType = std::move(type);
        }

    private:
        TypePtr _literalType = nullptr;
    };

    class NumberLiteral: public Literal {
    public:
        NumberLiteral(SourceRange sourceRange, double value):
            Literal(std::move(sourceRange),
                    std::make_shared<PrimitiveType>(canBe32Bit(value) ? PrimitiveTypeKind::F32 : PrimitiveTypeKind::F64)) {
            _value.f = value;
        }

        NumberLiteral(SourceRange sourceRange, int64_t value):
            Literal(std::move(sourceRange),
                    std::make_shared<PrimitiveType>(canBe32Bit(value) ? PrimitiveTypeKind::I32 : PrimitiveTypeKind::I64)) {
            _value.i = value;
        }

        NumberLiteral(SourceRange sourceRange, uint64_t value):
            Literal(std::move(sourceRange),
                    std::make_shared<PrimitiveType>(canBe32Bit(value) ? PrimitiveTypeKind::U32 : PrimitiveTypeKind::U64)) {
            _value.u = value;
        }

        float f32() const {
            return (float) _value.f;
        }

        double f64() const {
            return _value.f;
        }

        int32_t i32() const {
            return (int32_t) _value.i;
        }

        int64_t i64() const {
            return _value.i;
        }

        uint32_t u32() const {
            return (uint32_t) _value.u;
        }

        uint64_t u64() const {
            return _value.u;
        }

        void accept(ExprVisitor* visitor) override {
            visitor->visitNumberLiteral(this);
        };
    private:
        bool canBe32Bit(int64_t i) {
            return i == (int32_t) i;
        }

        bool canBe32Bit(uint64_t u) {
            return u == (uint32_t) u;
        }

        bool canBe32Bit(double d) {
            return d == (float) d;
        }
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
            Literal(std::move(sourceRange),
                    std::make_shared<PrimitiveType>(PrimitiveTypeKind::BOOL)),
                    _value(value) {
        }

        explicit BoolLiteral(bool value):
            Literal(SourceRange(),
                    std::make_shared<PrimitiveType>(PrimitiveTypeKind::BOOL)),
                    _value(value) {
        }

        bool value() const {
            return _value;
        }

        void accept(ExprVisitor* visitor) override {
            visitor->visitBoolLiteral(this);
        };
    private:
        bool _value;
    };

    class NullLiteral : public Literal {
    public:
        NullLiteral(SourceRange sourceRange) : 
            Literal(std::move(sourceRange), 
                std::make_shared<PointerType>(std::make_shared<PrimitiveType>(PrimitiveTypeKind::VOID))) {
        }

        void accept(ExprVisitor* visitor) override {
            visitor->visitNullLiteral(this);
        };
    };

    class StringLiteral: public Literal {
    public:
        StringLiteral(SourceRange sourceRange, std::string value):
            Literal(std::move(sourceRange),
                    std::make_shared<PointerType>(std::make_shared<PrimitiveType>(PrimitiveTypeKind::I8))),
                    _value(std::move(value)) {
        }

        const std::string& value() const {
            return _value;
        }

        void accept(ExprVisitor* visitor) override {
            visitor->visitStringLiteral(this);
        };
    private:
        std::string _value;
    };

    class CharacterLiteral: public Literal {
    public:
        CharacterLiteral(SourceRange sourceRange, char value):
            Literal(std::move(sourceRange),
                    std::make_shared<PrimitiveType>(PrimitiveTypeKind::I8)),
                    _value(value) {
        }

        char value() const {
            return _value;
        }

        void accept(ExprVisitor* visitor) override {
            visitor->visitCharacterLiteral(this);
        };
    private:
        char _value;
    };

    class ArrayLiteral: public Literal {
    public:
        ArrayLiteral(SourceRange sourceRange, std::vector<ExprPtr>&& values):
            Literal(std::move(sourceRange)),
            _values(values) {
        }

        std::vector<Expr*> values() const {
            std::vector<Expr*> values;
            for(auto& val : _values) {
                values.push_back(val.get());
            }
            return values;
        }

        void accept(ExprVisitor* visitor) override {
            visitor->visitArrayLiteral(this);
        }

    private:
        std::vector<ExprPtr> _values;
    };

    enum class LogicalOperation {
        OR,
        AND
    };
    
    class Logical : public Expr {
    public:
        Logical(SourceRange sourceRange, ExprPtr left, LogicalOperation op, ExprPtr right):
            Expr(ExprKind::LOGICAL, std::move(sourceRange)),
            _left(std::move(left)), _op(op), _right(std::move(right)) {
        }

        void accept(ExprVisitor* visitor) override {
            visitor->visitLogicalExpr(this);
        }

        Expr* left() const {
            return _left.get();
        }

        Expr* right() const {
            return _right.get();
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
        MINUS,
        DEREF,
        ADDRESS_OF
    };

    class Unary : public Expr {
    public:
        Unary(SourceRange sourceRange, UnaryOperation op, ExprPtr right):
            Expr(ExprKind::UNARY, std::move(sourceRange)),
            _op(op), _right(std::move(right)) {
        }

        void accept(ExprVisitor* visitor) override {
            visitor->visitUnaryExpr(this);
        }

        UnaryOperation op() const {
            return _op;
        }

        Expr* right() const {
            return _right.get();
        }

    private:
        UnaryOperation _op;
        ExprPtr _right;
    };

    class SizeOf : public Expr {
    public:
        SizeOf(SourceRange sourceRange, TypePtr right):
        Expr(ExprKind::SIZE_OF, std::move(sourceRange)),
        _right(std::move(right)) {
        }

        void accept(ExprVisitor* visitor) override {
            visitor->visitSizeOfExpr(this);
        }

        Type* right() const {
            return _right.get();
        }

    private:
        TypePtr _right;
    };

    class Cast : public Expr {
    public:
        Cast(SourceRange sourceRange, TypePtr targetType, ExprPtr right):
            Expr(ExprKind::CAST, std::move(sourceRange)),
            _targetType(std::move(targetType)),
            _right(std::move(right)){
        }

        void accept(ExprVisitor* visitor) override {
            visitor->visitCastExpr(this);
        }

        Type* targetType() const {
            return _targetType.get();
        }

        Expr* right() const {
            return _right.get();
        }

    private:
        TypePtr _targetType;
        ExprPtr _right;
    };

    class Stmt;

    class Variable : public Expr {
    public:
        Variable(SourceRange sourceRange, std::string name):
            Expr(ExprKind::VARIABLE, std::move(sourceRange)),
            _name(std::move(name)) {
        }

        void accept(ExprVisitor* visitor) override {
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