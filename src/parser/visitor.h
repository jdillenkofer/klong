#pragma once

namespace klong {
    // Stmt
    class Block;
    class Expression;
    class Function;
    class If;
    class Print;
    class Return;
    class Let;
    class Const;
    class While;
    
    // Expr
    class Assign;
    class Binary;
    class Call;
    class Grouping;
    class Literal;
    class Logical;
    class Unary;
    class Variable;

    class Visitor {
        public:
            // Stmt
            virtual void visitBlockStmt(Block* stmt) = 0;
            virtual void visitExpressionStmt(Expression* stmt) = 0;
            virtual void visitFunctionStmt(Function* stmt) = 0;
            virtual void visitIfStmt(If* stmt) = 0;
            virtual void visitPrintStmt(Print* stmt) = 0;
            virtual void visitReturnStmt(Return* stmt) = 0;
            virtual void visitLetStmt(Let* stmt) = 0;
            virtual void visitConstStmt(Const* stmt) = 0;
            virtual void visitWhileStmt(While* stmt) = 0;

            // Expr
            virtual void visitAssignExpr(Assign* expr) = 0;
            virtual void visitBinaryExpr(Binary* expr) = 0;
            virtual void visitCallExpr(Call* expr) = 0;
            virtual void visitGroupingExpr(Grouping* expr) = 0;
            virtual void visitLiteralExpr(Literal* expr) = 0;
            virtual void visitLogicalExpr(Logical* expr) = 0;
            virtual void visitUnaryExpr(Unary* expr) = 0;
            virtual void visitVariableExpr(Variable* expr) = 0;
    };
}