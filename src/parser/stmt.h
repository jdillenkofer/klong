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
            Stmt(StatementKind kind, SourceRange sourceRange): _kind(kind), _sourceRange(sourceRange) {
            }
            virtual ~Stmt() = default;

            virtual void accept(Visitor* visitor) = 0;

            StatementKind kind() const {
                return _kind;
            }

            SourceRange sourceRange() const {
                return _sourceRange;
            }

        private:
            StatementKind _kind;
            SourceRange _sourceRange;
    };

    using StmtPtr = std::shared_ptr<Stmt>;
    using ExprPtr = std::shared_ptr<Expr>;

    class Block : public Stmt {
        public:
            Block(SourceRange sourceRange, std::vector<StmtPtr>&& statements):
                Stmt(StatementKind::BLOCK, sourceRange),
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
            Expression(SourceRange sourceRange, ExprPtr expression):
                Stmt(StatementKind::EXPRESSION, sourceRange),
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
            Function(SourceRange sourceRange, Token name, std::vector<Token>&& params,
                std::shared_ptr<FunctionType> functionType, std::vector<StmtPtr>&& body):
                Stmt(StatementKind::FUNCTION, sourceRange),
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
            If(SourceRange sourceRange, ExprPtr condition, StmtPtr thenBranch, StmtPtr elseBranch):
                Stmt(StatementKind::IF, sourceRange),
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
            Print(SourceRange sourceRange, ExprPtr expression):
                Stmt(StatementKind::PRINT, sourceRange),
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
            Return(SourceRange sourceRange, Token keyword, ExprPtr value):
                Stmt(StatementKind::RETURN, sourceRange),
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
            Let(SourceRange sourceRange, Token name, TypePtr type, ExprPtr initializer):
                Stmt(StatementKind::LET, sourceRange),
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
            Const(SourceRange sourceRange, Token name, TypePtr type, ExprPtr initializer):
                Stmt(StatementKind::CONST, sourceRange),
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
            While(SourceRange sourceRange, ExprPtr condition, StmtPtr body):
                Stmt(StatementKind::WHILE, sourceRange),
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
            For(SourceRange sourceRange, StmtPtr initializer, ExprPtr condition, ExprPtr increment, StmtPtr body):
                Stmt(StatementKind::FOR, sourceRange),
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

    enum class CommentType {
        BLOCK,
        LINE
    };

    class Comment : public Stmt {
        public:
            Comment(SourceRange sourceRange, const std::string& text, CommentType type):
                Stmt(StatementKind::COMMENT, sourceRange),
                _text(text),
                _type(type){

            }

            void accept(Visitor* visitor) {
                visitor->visitCommentStmt(this);
            }

            const std::string& text() const {
                return _text;
            }
        private:
            std::string _text;
            CommentType _type;
    };
}