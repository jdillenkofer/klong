#pragma once

#include "visitor.h"
#include "stmt.h"
#include "expr.h"

namespace klong {
    class Resolver : public Visitor {
        public:
            Resolver() = default;

            void resolve(const StmtPtr& stmt);
            void resolve(const ExprPtr& expr);

            // Module
            void visitModule(Module* module) override;

            // Stmt
            void visitBlockStmt(Block* stmt) override;
            void visitExpressionStmt(Expression* stmt) override;
            void visitFunctionStmt(Function* stmt) override;
            void visitIfStmt(If* stmt) override;
            void visitPrintStmt(Print* stmt) override;
            void visitReturnStmt(Return* stmt) override;
            void visitLetStmt(Let* stmt) override;
            void visitConstStmt(Const* stmt) override;
            void visitWhileStmt(While* stmt) override;
            void visitForStmt(For* stmt) override;
            void visitCommentStmt(Comment* expr) override;

            // Expr
            void visitAssignExpr(Assign* expr) override;
            void visitBinaryExpr(Binary* expr) override;
            void visitCallExpr(Call* expr) override;
            void visitGroupingExpr(Grouping* expr) override;
            void visitLogicalExpr(Logical* expr) override;
            void visitUnaryExpr(Unary* expr) override;
            void visitVariableExpr(Variable* expr) override;

            // Literals
            void visitNumberLiteral(NumberLiteral* expr) override;
            void visitBoolLiteral(BoolLiteral* expr) override;
            void visitStringLiteral(StringLiteral* expr) override;
            void visitCharacterLiteral(CharacterLiteral* expr) override;

            // Types
            void visitFunctionType(FunctionType* type) override;
            void visitPrimitiveType(PrimitiveType *type) override;
            void visitSimpleType(SimpleType *type) override;
    };
}