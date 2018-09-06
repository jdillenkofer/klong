#pragma once

namespace klong {
    // Module
    class Module;

    // Stmt
    class Block;
    class Expression;
    class Function;
    class Parameter;
    class If;
    class Print;
    class Return;
    class Let;
    class Const;
    class While;
    class For;
    class Comment;
    
    // Expr
    class Assign;
    class Binary;
    class Call;
    class Grouping;
    class Logical;
    class Unary;
    class Variable;

    // Literals
    class NumberLiteral;
    class BoolLiteral;
    class StringLiteral;
    class CharacterLiteral;

    // Types
    class FunctionType;
    class PrimitiveType;
    class SimpleType;

    class Visitor {
        public:
            // Module
            virtual void visitModule(Module* module) = 0;

            // Stmt
            virtual void visitBlockStmt(Block* stmt) = 0;
            virtual void visitExpressionStmt(Expression* stmt) = 0;
            virtual void visitFunctionStmt(Function* stmt) = 0;
            virtual void visitParameterStmt(Parameter* stmt) = 0;
            virtual void visitIfStmt(If* stmt) = 0;
            virtual void visitPrintStmt(Print* stmt) = 0;
            virtual void visitReturnStmt(Return* stmt) = 0;
            virtual void visitLetStmt(Let* stmt) = 0;
            virtual void visitConstStmt(Const* stmt) = 0;
            virtual void visitWhileStmt(While* stmt) = 0;
            virtual void visitForStmt(For* stmt) = 0;
            virtual void visitCommentStmt(Comment* expr) = 0;

            // Expr
            virtual void visitAssignExpr(Assign* expr) = 0;
            virtual void visitBinaryExpr(Binary* expr) = 0;
            virtual void visitCallExpr(Call* expr) = 0;
            virtual void visitGroupingExpr(Grouping* expr) = 0;
            virtual void visitLogicalExpr(Logical* expr) = 0;
            virtual void visitUnaryExpr(Unary* expr) = 0;
            virtual void visitVariableExpr(Variable* expr) = 0;

            // Literals
            virtual void visitNumberLiteral(NumberLiteral* expr) = 0;
            virtual void visitBoolLiteral(BoolLiteral* expr) = 0;
            virtual void visitStringLiteral(StringLiteral* expr) = 0;
            virtual void visitCharacterLiteral(CharacterLiteral* expr) = 0;

            // Types
            virtual void visitFunctionType(FunctionType* type) = 0;
            virtual void visitPrimitiveType(PrimitiveType *type) = 0;
            virtual void visitSimpleType(SimpleType *type) = 0;
    };
}