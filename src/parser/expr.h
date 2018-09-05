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
            Expr(ExprKind kind): _kind(kind) {

            }
            virtual ~Expr() = default;
            virtual void accept(Visitor* visitor) = 0;
            ExprKind kind() const {
                return _kind;
            }
        private:
            ExprKind _kind;
    };

    using ExprPtr = std::shared_ptr<Expr>;

    class Assign : public Expr {
        public:
            Assign(Token name, ExprPtr value): 
                Expr(ExprKind::ASSIGN),
                _name(name), _value(value) {

            }

            void accept(Visitor* visitor) {
                visitor->visitAssignExpr(this);
            }

        private:
            Token _name;
            ExprPtr _value;
    };
    
    class Binary : public Expr {
        public:
            Binary(ExprPtr left, Token op, ExprPtr right):
                Expr(ExprKind::BINARY),
                _left(left), _op(op), _right(right) {

            }

            void accept(Visitor* visitor) {
                visitor->visitBinaryExpr(this);
            }

        private:
            ExprPtr _left;
            Token _op;
            ExprPtr _right;
    };
    
    class Call : public Expr {
        public:
            Call(ExprPtr callee, Token paren, std::vector<ExprPtr>&& args):
                Expr(ExprKind::CALL),
                _callee(callee), _paren(paren), _args(args) {

            }

            void accept(Visitor* visitor) {
                visitor->visitCallExpr(this);
            }

        private:
            ExprPtr _callee;
            Token _paren;
            std::vector<ExprPtr> _args;
    };
    
    class Grouping : public Expr {
        public:
            Grouping(ExprPtr expr):
                Expr(ExprKind::GROUPING),
                _expr(expr) {

            }

            void accept(Visitor* visitor) {
                visitor->visitGroupingExpr(this);
            }

        private:
            ExprPtr _expr;
    };
    
    class Literal : public Expr {
        public:
            Literal(Token value):
                Expr(ExprKind::LITERAL),
                _value(value) {

            }

            void accept(Visitor* visitor) {
                visitor->visitLiteralExpr(this);
            }

        private:
            Token _value;
    };
    
    class Logical : public Expr {
        public:
            Logical(ExprPtr left, Token op, ExprPtr right):
                Expr(ExprKind::LOGICAL),
                _left(left), _op(op), _right(right) {

            }

            void accept(Visitor* visitor) {
                visitor->visitLogicalExpr(this);
            }

        private:
            ExprPtr _left;
            Token _op;
            ExprPtr _right;
    };
    
    class Unary : public Expr {
        public:
            Unary(Token op, ExprPtr right):
                Expr(ExprKind::UNARY),
                _op(op), _right(right) {

            }

            void accept(Visitor* visitor) {
                visitor->visitUnaryExpr(this);
            }

        private:
            Token _op;
            ExprPtr _right;
    };

    class Variable : public Expr {
        public:
            Variable(Token name):
                Expr(ExprKind::VARIABLE),
                _name(name) {

            }

            void accept(Visitor* visitor) {
                visitor->visitVariableExpr(this);
            }

            Token name() {
                return _name;
            }

        private:
            Token _name;
    };
}