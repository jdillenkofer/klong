#pragma once

#include "visitor.h"
#include "lexer/token.h"
#include "type.h"

#include <vector>
#include <memory>
#include <utility>

namespace klong {

    class Expr;

    enum class StatementKind {
        BLOCK,
        EXPRESSION,
        EXT_DECL,
        FUNCTION,
        PARAMETER,
        IF,
        PRINT,
        RETURN,
        VAR_DECL,
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

        std::vector<StmtPtr> statements() const {
            return _statements;
        }

    private:
        std::vector<StmtPtr> _statements;
    };

    class Expression : public Stmt {
    public:
        Expression(SourceRange sourceRange, ExprPtr expression):
            Stmt(StatementKind::EXPRESSION, sourceRange),
            _expression(std::move(expression)) {
        }

        void accept(Visitor* visitor) {
            visitor->visitExpressionStmt(this);
        }

        ExprPtr expression() const {
            return _expression;
        }

    private:
        ExprPtr _expression;
    };

    class ExternalDeclaration : public Stmt {
    public:
        ExternalDeclaration(SourceRange sourceRange, std::string name, TypePtr type):
            Stmt(StatementKind::EXT_DECL, sourceRange),
            _name(std::move(name)),
            _type(type) {
        }

        void accept(Visitor* visitor) {
            visitor->visitExtDeclStmt(this);
        }

        std::string name() const {
            return _name;
        }

        TypePtr type() const {
            return _type;
        }

    private:
        std::string _name;
        TypePtr _type;
    };

    class Parameter : public Stmt {
    public:
        Parameter(SourceRange sourceRange, std::string name, TypePtr type):
            Stmt(StatementKind::PARAMETER, sourceRange),
            _name(std::move(name)),
            _type(type) {
        }

        void accept(Visitor* visitor) {
            visitor->visitParameterStmt(this);
        }

        std::string name() const {
            return _name;
        }

        TypePtr type() const {
            return _type;
        }

    private:
        std::string _name;
        TypePtr _type;
    };

    using ParameterPtr = std::shared_ptr<Parameter>;

    class Function : public Stmt {
    public:
        Function(SourceRange sourceRange, std::string name, std::vector<ParameterPtr>&& params,
            std::shared_ptr<FunctionType> functionType, std::vector<StmtPtr>&& body, bool isPublic):
            Stmt(StatementKind::FUNCTION, sourceRange),
            _name(std::move(name)),
            _params(params),
            _functionType(std::move(functionType)),
            _body(body),
            _isPublic(isPublic) {
        }

        void accept(Visitor* visitor) {
            visitor->visitFunctionStmt(this);
        }

        std::string name() const {
            return _name;
        }

        std::vector<ParameterPtr> params() const {
            return _params;
        }

        std::shared_ptr<FunctionType> functionType() const {
            return _functionType;
        }

        std::vector<StmtPtr> body() const {
            return _body;
        }

        bool isPublic() const {
            return _isPublic;
        }

    private:
        std::string _name;
        std::vector<ParameterPtr> _params;
        std::shared_ptr<FunctionType> _functionType;
        std::vector<StmtPtr> _body;
        bool _isPublic;
    };

    class If : public Stmt {
    public:
        If(SourceRange sourceRange, ExprPtr condition, StmtPtr thenBranch, StmtPtr elseBranch):
            Stmt(StatementKind::IF, sourceRange),
            _condition(std::move(condition)),
            _thenBranch(std::move(thenBranch)),
            _elseBranch(std::move(elseBranch)) {
        }

        void accept(Visitor* visitor) {
            visitor->visitIfStmt(this);
        }

        ExprPtr condition() const {
            return _condition;
        }

        StmtPtr thenBranch() const {
            return _thenBranch;
        }

        StmtPtr elseBranch() const {
            return _elseBranch;
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
            _expression(std::move(expression)) {
        }

        void accept(Visitor* visitor) {
            visitor->visitPrintStmt(this);
        }

        ExprPtr expression() const {
            return _expression;
        }

    private:
        ExprPtr _expression;
    };

    class Return : public Stmt {
    public:
        Return(SourceRange sourceRange, ExprPtr value):
            Stmt(StatementKind::RETURN, sourceRange), _value(std::move(value)) {
        }

        void accept(Visitor* visitor) {
            visitor->visitReturnStmt(this);
        }

        ExprPtr value() const {
            return _value;
        }

    private:
        ExprPtr _value;
    };

    class VariableDeclaration : public Stmt {
    public:
        VariableDeclaration(SourceRange sourceRange,
                std::string name,
                TypePtr type,
                ExprPtr initializer,
                bool isPublic,
                bool isConst,
                bool isGlobal):
                Stmt(StatementKind::VAR_DECL, sourceRange),
                _name(std::move(name)),
                _type(std::move(type)),
                _initializer(std::move(initializer)),
                _isPublic(isPublic),
                _isConst(isConst),
                _isGlobal(isGlobal) {
        }

        void accept(Visitor* visitor) {
            visitor->visitVarDeclStmt(this);
        }

        std::string name() const {
            return _name;
        }

        TypePtr type() const {
            return _type;
        }

        void type(TypePtr type) {
            _type = std::move(type);
        }

        ExprPtr initializer() const {
            return _initializer;
        }

        bool isPublic() const {
            return _isPublic;
        }

        bool isConst() const {
            return _isConst;
        }

        bool isGlobal() const {
            return _isGlobal;
        }

    private:
        std::string _name;
        TypePtr _type;
        ExprPtr _initializer;
        bool _isPublic;
        bool _isConst;
        bool _isGlobal;
    };

    class While : public Stmt {
    public:
        While(SourceRange sourceRange, ExprPtr condition, StmtPtr body):
            Stmt(StatementKind::WHILE, sourceRange),
            _condition(std::move(condition)), _body(std::move(body)) {
        }

        void accept(Visitor* visitor) {
            visitor->visitWhileStmt(this);
        }

        ExprPtr condition() const {
            return _condition;
        }

        StmtPtr body() const {
            return _body;
        }

    private:
        ExprPtr _condition;
        StmtPtr _body;
    };

    class For : public Stmt {
    public:
        For(SourceRange sourceRange, StmtPtr initializer, ExprPtr condition, ExprPtr increment, StmtPtr body):
            Stmt(StatementKind::FOR, sourceRange),
            _initializer(std::move(initializer)),
            _condition(std::move(condition)),
            _increment(std::move(increment)),
            _body(std::move(body)) {
        }

        void accept(Visitor* visitor) {
            visitor->visitForStmt(this);
        }

        StmtPtr initializer() const {
            return _initializer;
        }

        ExprPtr condition() const {
            return _condition;
        };

        ExprPtr increment() const {
            return _increment;
        }

        StmtPtr body() const {
            return _body;
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
        Comment(SourceRange sourceRange, std::string text, CommentType type):
            Stmt(StatementKind::COMMENT, sourceRange),
            _text(std::move(text)),
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