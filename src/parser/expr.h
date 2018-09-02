#pragma once

#include "visitor.h"
#include "../lexer/token.h"

#include <vector>
#include <memory>

namespace klong {
    
    class Expr {
        public:
            virtual ~Expr() = 0;
            virtual void accept(Visitor* visitor) = 0;
    };

    using ExprPtr = std::shared_ptr<Expr>;

    class Assign : Expr {
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
    
    class Binary : Expr {
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
    
    class Call : Expr {
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
    
    class Grouping : Expr {
        public:
            Grouping(ExprPtr expr): _expr(expr) {

            }

            void accept(Visitor* visitor) {
                visitor->visitGroupingExpr(this);
            }
        private:
            ExprPtr _expr;
    };
    
    class Literal : Expr {
        public:
            Literal(Token value): _value(value) {

            }

            void accept(Visitor* visitor) {
                visitor->visitLiteralExpr(this);
            }

        private:
            Token _value;
    };
    
    class Logical : Expr {
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
    
    class Unary : Expr {
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

    class Variable : Expr {
        public:
            Variable(Token name): 
                _name(name) {

            }

            void accept(Visitor* visitor) {
                visitor->visitVariableExpr(this);
            }
        private:
            Token _name;
    };
}
