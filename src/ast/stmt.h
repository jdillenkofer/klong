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

        virtual void accept(StmtVisitor* visitor) = 0;

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

        void accept(StmtVisitor* visitor) {
            visitor->visitBlockStmt(this);
        }

        std::vector<Stmt*> statements() const {
            std::vector<Stmt*> stmts;
            for (auto& stmt : _statements) {
                stmts.push_back(stmt.get());
            }
            return stmts;
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

        void accept(StmtVisitor* visitor) {
            visitor->visitExpressionStmt(this);
        }

        Expr* expression() const {
            return _expression.get();
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

        void accept(StmtVisitor* visitor) {
            visitor->visitExtDeclStmt(this);
        }

        std::string name() const {
            return _name;
        }

        Type* type() const {
            return _type.get();
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

        void accept(StmtVisitor* visitor) {
            visitor->visitParameterStmt(this);
        }

        std::string name() const {
            return _name;
        }

        Type* type() const {
            return _type.get();
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

        void accept(StmtVisitor* visitor) {
            visitor->visitFunctionStmt(this);
        }

        std::string name() const {
            return _name;
        }

        std::vector<Parameter*> params() const {
            std::vector<Parameter*> params;
            for (auto& param : _params) {
                params.push_back(param.get());
            }
            return params;
        }

        FunctionType* functionType() const {
            return _functionType.get();
        }

        std::vector<Stmt*> body() const {
            std::vector<Stmt*> body;
            for (auto& stmt : _body) {
                body.push_back(stmt.get());
            }
            return body;
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
            _elseBranch(std::move(elseBranch)),
			_isMergeUnreachable(false) {
        }

        void accept(StmtVisitor* visitor) {
            visitor->visitIfStmt(this);
        }

        Expr* condition() const {
            return _condition.get();
        }

        Stmt* thenBranch() const {
            return _thenBranch.get();
        }

        Stmt* elseBranch() const {
            return _elseBranch.get();
        }

        bool isMergeUnreachable() const {
            return _isMergeUnreachable;
        }

        void setMergeUnreachable() {
            _isMergeUnreachable = true;
        }

    private:
        ExprPtr _condition;
        StmtPtr _thenBranch;
        StmtPtr _elseBranch;
        bool _isMergeUnreachable;
    };

    class Return : public Stmt {
    public:
        Return(SourceRange sourceRange, ExprPtr value):
            Stmt(StatementKind::RETURN, sourceRange), _value(std::move(value)) {
        }

        void accept(StmtVisitor* visitor) {
            visitor->visitReturnStmt(this);
        }

        Expr* value() const {
            return _value.get();
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

        void accept(StmtVisitor* visitor) {
            visitor->visitVarDeclStmt(this);
        }

        std::string name() const {
            return _name;
        }

        Type* type() const {
            return _type.get();
        }

        void type(TypePtr type) {
            _type = std::move(type);
        }

        Expr* initializer() const {
            return _initializer.get();
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

        void accept(StmtVisitor* visitor) {
            visitor->visitWhileStmt(this);
        }

        Expr* condition() const {
            return _condition.get();
        }

        Stmt* body() const {
            return _body.get();
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

        void accept(StmtVisitor* visitor) {
            visitor->visitForStmt(this);
        }

        Stmt* initializer() const {
            return _initializer.get();
        }

        Expr* condition() const {
            return _condition.get();
        };

        Expr* increment() const {
            return _increment.get();
        }

        Stmt* body() const {
            return _body.get();
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

        void accept(StmtVisitor* visitor) {
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