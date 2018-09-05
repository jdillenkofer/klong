#pragma once

#include "visitor.h"
#include "../lexer/token.h"
#include "type.h"

#include <vector>
#include <memory>
#include <utility>

namespace klong {

    class Expr;

    enum class StatementKind {
        BLOCK,
        EXPRESSION,
        FUNCTION,
        IF,
        PRINT,
        RETURN,
        LET,
        CONST,
        WHILE,
        FOR,
        COMMENT
    };
    
    class Stmt {
        public:
            Stmt(StatementKind kind): _kind(kind) {
            }
            virtual ~Stmt() = default;
            virtual void accept(Visitor* visitor) = 0;
            StatementKind kind() const {
                return _kind;
            }
        private:
            StatementKind _kind;
    };

    using StmtPtr = std::shared_ptr<Stmt>;
    using ExprPtr = std::shared_ptr<Expr>;

    class Block : public Stmt {
        public:
            Block(std::vector<StmtPtr>&& statements):
                Stmt(StatementKind::BLOCK),
                _statements(statements) {

            }

            void accept(Visitor* visitor) {
                visitor->visitBlockStmt(this);
            }

        private:
            std::vector<StmtPtr> _statements;
    };

    class Expression : public Stmt {
        public:
            Expression(ExprPtr expression):
                Stmt(StatementKind::EXPRESSION),
                _expression(expression) {

            }

            void accept(Visitor* visitor) {
                visitor->visitExpressionStmt(this);
            }

        private:
            ExprPtr _expression;
    };

    class Function : public Stmt {
        public:
            Function(Token name, std::vector<Token>&& params, 
                std::shared_ptr<FunctionType> functionType, std::vector<StmtPtr>&& body):
                Stmt(StatementKind::FUNCTION),
                _name(name),
                _params(params),
                _functionType(functionType),
                _body(body) {

            }
            void accept(Visitor* visitor) {
                visitor->visitFunctionStmt(this);
            }

        private:
            Token _name;
            std::vector<Token> _params;
            std::shared_ptr<FunctionType> _functionType;
            std::vector<StmtPtr> _body;
    };

    class If : public Stmt {
        public:
            If(ExprPtr condition, StmtPtr thenBranch, StmtPtr elseBranch):
                Stmt(StatementKind::IF),
                _condition(condition),
                _thenBranch(thenBranch),
                _elseBranch(elseBranch) {

            }
            void accept(Visitor* visitor) {
                visitor->visitIfStmt(this);
            }

        private:
            ExprPtr _condition;
            StmtPtr _thenBranch;
            StmtPtr _elseBranch;
    };

    class Print : public Stmt {
        public:
            Print(ExprPtr expression):
                Stmt(StatementKind::PRINT),
                _expression(expression) {

            }

            void accept(Visitor* visitor) {
                visitor->visitPrintStmt(this);
            }

        private:
            ExprPtr _expression;
    };

    class Return : public Stmt {
        public:
            Return(Token keyword, ExprPtr value):
                Stmt(StatementKind::RETURN),
                _keyword(keyword), _value(value) {

            }

            void accept(Visitor* visitor) {
                visitor->visitReturnStmt(this);
            }

        private:
            Token _keyword;
            ExprPtr _value;
    };

    class Let : public Stmt {
        public:
            Let(Token name, TypePtr type, ExprPtr initializer):
                Stmt(StatementKind::LET),
                _name(name),
                _type(type),
                _initializer(initializer) {

            }

            void accept(Visitor* visitor) {
                visitor->visitLetStmt(this);
            }

        private:
            Token _name;
            TypePtr _type;
            ExprPtr _initializer;
    };

    class Const : public Stmt {
        public:
            Const(Token name, TypePtr type, ExprPtr initializer):
                Stmt(StatementKind::CONST),
                _name(name),
                _type(type),
                _initializer(initializer) {

            }

            void accept(Visitor* visitor) {
                visitor->visitConstStmt(this);
            }

        private:
            Token _name;
            TypePtr _type;
            ExprPtr _initializer;
    };

    class While : public Stmt {
        public:
            While(ExprPtr condition, StmtPtr body):
                Stmt(StatementKind::WHILE),
                _condition(condition), _body(body) {
                
            }

            void accept(Visitor* visitor) {
                visitor->visitWhileStmt(this);
            }

        private:
            ExprPtr _condition;
            StmtPtr _body;
    };

    class For : public Stmt {
        public:
            For(StmtPtr initializer, ExprPtr condition, ExprPtr increment, StmtPtr body):
                Stmt(StatementKind::FOR),
                _initializer(initializer),
                _condition(condition),
                _increment(increment),
                _body(body) {
                
            }

            void accept(Visitor* visitor) {
                visitor->visitForStmt(this);
            }

        private:
            StmtPtr _initializer;
            ExprPtr _condition;
            ExprPtr _increment;
            StmtPtr _body;
    };

    class Comment : public Stmt {
        public:
            Comment(Token name):
                Stmt(StatementKind::COMMENT),
                _name(name) {

            }

            void accept(Visitor* visitor) {
                visitor->visitCommentStmt(this);
            }

            Token name() {
                return _name;
            }
        private:
            Token _name;
    };
}