#pragma once

#include "visitor.h"
#include "../lexer/token.h"

#include <vector>
#include <memory>

namespace klong {

    class Expr;
    
    class Stmt {
        public:
            virtual ~Stmt() = 0;
            virtual void accept(Visitor* visitor) = 0;
    };

    using StmtPtr = std::shared_ptr<Stmt>;
    using ExprPtr = std::shared_ptr<Expr>;

    class Block : Stmt {
        public:
            Block(std::vector<StmtPtr>&& statements):
                _statements(statements) {

            }

            void accept(Visitor* visitor) {
                visitor->visitBlockStmt(this);
            }
        private:
            std::vector<StmtPtr> _statements;
    };

    class Expression : Stmt {
        public:
            Expression(ExprPtr expression): 
                _expression(expression) {

            }
            void accept(Visitor* visitor) {
                visitor->visitExpressionStmt(this);
            }
        private:
            ExprPtr _expression;
    };

    class Function : Stmt {
        public:
            Function(Token name, std::vector<Token> params, std::vector<StmtPtr> body):
                _name(name), _params(params), _body(body) {

            }
            void accept(Visitor* visitor) {
                visitor->visitFunctionStmt(this);
            }
        private:
            Token _name;
            std::vector<Token> _params;
            std::vector<StmtPtr> _body;
    };

    class If : Stmt {
        public:
            If(ExprPtr condition, StmtPtr thenBranch, StmtPtr elseBranch):
                _condition(condition), _thenBranch(thenBranch), _elseBranch(elseBranch) {

            }
            void accept(Visitor* visitor) {
                visitor->visitIfStmt(this);
            }
        private:
            ExprPtr _condition;
            StmtPtr _thenBranch;
            StmtPtr _elseBranch;
    };

    class Print : Stmt {
        public:
            Print(ExprPtr expression): _expression(expression) {

            }
            void accept(Visitor* visitor) {
                visitor->visitPrintStmt(this);
            }
        private:
            ExprPtr _expression;
    };

    class Return : Stmt {
        public:
            Return(Token keyword, ExprPtr value): 
            _keyword(keyword), _value(value) {

            }
            void accept(Visitor* visitor) {
                visitor->visitReturnStmt(this);
            }
        private:
            Token _keyword;
            ExprPtr _value;
    };

    class Let : Stmt {
        public:
            Let(Token name, ExprPtr initializer):
                _name(name), _initializer(initializer) {

            }
            void accept(Visitor* visitor) {
                visitor->visitLetStmt(this);
            }
        private:
            Token _name;
            ExprPtr _initializer;
    };

    class Const : Stmt {
        public:
            Const(Token name, ExprPtr initializer): 
                _name(name), _initializer(initializer) {

            }
            void accept(Visitor* visitor) {
                visitor->visitConstStmt(this);
            }
        private:
            Token _name;
            ExprPtr _initializer;
    };

    class While : Stmt {
        public:
            While(ExprPtr condition, StmtPtr body): 
                _condition(condition), _body(body) {
                
            }
            void accept(Visitor* visitor) {
                visitor->visitWhileStmt(this);
            }
        private:
            ExprPtr _condition;
            StmtPtr _body;
    };
}