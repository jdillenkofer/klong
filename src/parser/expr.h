#pragma once

#include "visitor.h"
#include "../lexer/token.h"

#include <vector>
#include <memory>

namespace klong {
    
    class Expr {
        public:
            virtual ~Expr() = default;
            virtual void accept(Visitor* visitor) = 0;
    };

    using ExprPtr = std::shared_ptr<Expr>;

    class Assign : public Expr {
        public:
            Assign(Token name, ExprPtr value): 
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
            Grouping(ExprPtr expr): _expr(expr) {

            }

            void accept(Visitor* visitor) {
                visitor->visitGroupingExpr(this);
            }

        private:
            ExprPtr _expr;
    };
    
    class Literal : public Expr {
        public:
            Literal(Token value): _value(value) {

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