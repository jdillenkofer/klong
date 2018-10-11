#pragma once

#include "common/result.h"
#include "ast/visitor.h"
#include "ast/module.h"
#include "ast/stmt.h"
#include "ast/expr.h"

namespace klong {
    class TypeCheckException : public std::exception {
    public:
        TypeCheckException(SourceRange sourceRange, std::string message):
            _sourceRange(sourceRange), _message(std::move(message)) {
        }

        SourceRange sourceRange() const {
            return _sourceRange;
        }

        const char* what () const noexcept override {
            return _message.c_str();
        }

    private:
        SourceRange _sourceRange;
        std::string _message;
    };

    class TypeCheckVisitor : public StmtVisitor, public ExprVisitor {
    public:
        TypeCheckVisitor() = default;

        // Module
        void visitModule(Module* module) override;

        // Stmt
        void visitBlockStmt(Block* stmt) override;
        void visitExpressionStmt(Expression* stmt) override;
        void visitExtDeclStmt(ExternalDeclaration* stmt) override;
        void visitFunctionStmt(Function* stmt) override;
        void visitParameterStmt(Parameter* stmt) override;
        void visitIfStmt(If* stmt) override;
        void visitReturnStmt(Return* stmt) override;
        void visitVarDeclStmt(VariableDeclaration* stmt) override;
        void visitWhileStmt(While* stmt) override;
        void visitForStmt(For* stmt) override;
        void visitCommentStmt(Comment* stmt) override;

        // Expr
        void visitAssignExpr(Assign* expr) override;
        void visitBinaryExpr(Binary* expr) override;
        void visitCallExpr(Call* expr) override;
        void visitGroupingExpr(Grouping* expr) override;
        void visitLogicalExpr(Logical* expr) override;
        void visitUnaryExpr(Unary* expr) override;
        void visitSizeOfExpr(SizeOf* expr) override;
        void visitVariableExpr(Variable* expr) override;

        // Literals
        void visitNumberLiteral(NumberLiteral* expr) override;
        void visitBoolLiteral(BoolLiteral* expr) override;
        void visitStringLiteral(StringLiteral* expr) override;
        void visitCharacterLiteral(CharacterLiteral* expr) override;

        Result<ModulePtr, TypeCheckException> getResult() const;

    private:
        void check(const std::vector<StmtPtr>& statements);
        void check(Stmt* stmt);
        void check(Expr* expr);
        bool getAndResetReturnsValue();

        bool isBoolean(Expr* expr);
        bool isInteger(Expr* expr);
        bool isFloat(Expr* expr);
        bool isString(Expr* expr);
        bool isPointer(Expr* expr);
    private:
        Function* currentFunction = nullptr;
        Result<ModulePtr, TypeCheckException> _result;
        bool _returnsValue = false;
    };
}