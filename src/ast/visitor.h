#pragma once

namespace klong {
    // Module
    class Module;

    // Stmt
    class Block;
    class Expression;
    class Function;
    class ExternalDeclaration;
    class Parameter;
    class If;
    class Return;
    class VariableDeclaration;
    class While;
    class For;
    class Break;
    class Continue;
    class Defer;
    class Comment;

    class StmtVisitor {
    public:
        // Module
        virtual void visitModule(Module* module) = 0;

        // Stmt
        virtual void visitBlockStmt(Block* stmt) = 0;
        virtual void visitExpressionStmt(Expression* stmt) = 0;
        virtual void visitExtDeclStmt(ExternalDeclaration* stmt) = 0;
        virtual void visitFunctionStmt(Function* stmt) = 0;
        virtual void visitParameterStmt(Parameter* stmt) = 0;
        virtual void visitIfStmt(If* stmt) = 0;
        virtual void visitReturnStmt(Return* stmt) = 0;
        virtual void visitVarDeclStmt(VariableDeclaration* stmt) = 0;
        virtual void visitWhileStmt(While* stmt) = 0;
        virtual void visitForStmt(For* stmt) = 0;
        virtual void visitBreakStmt(Break* stmt) = 0;
        virtual void visitContinueStmt(Continue* stmt) = 0;
        virtual void visitDeferStmt(Defer* stmt) = 0;
        virtual void visitCommentStmt(Comment* expr) = 0;
    };
    
    // Expr
    class Assign;
    class Binary;
    class Call;
    class Grouping;
	class Subscript;
    class Logical;
    class Unary;
    class SizeOf;
    class Cast;
    class Variable;

    // Literals
    class NumberLiteral;
    class BoolLiteral;
    class StringLiteral;
    class CharacterLiteral;
    class ArrayLiteral;


    class ExprVisitor {
    public:
        // Expr
        virtual void visitAssignExpr(Assign* expr) = 0;
        virtual void visitBinaryExpr(Binary* expr) = 0;
        virtual void visitCallExpr(Call* expr) = 0;
        virtual void visitGroupingExpr(Grouping* expr) = 0;
		virtual void visitSubscriptExpr(Subscript* expr) = 0;
        virtual void visitLogicalExpr(Logical* expr) = 0;
        virtual void visitUnaryExpr(Unary* expr) = 0;
        virtual void visitSizeOfExpr(SizeOf* expr) = 0;
        virtual void visitCastExpr(Cast* expr) = 0;
        virtual void visitVariableExpr(Variable* expr) = 0;

        // Literals
        virtual void visitNumberLiteral(NumberLiteral* expr) = 0;
        virtual void visitBoolLiteral(BoolLiteral* expr) = 0;
        virtual void visitStringLiteral(StringLiteral* expr) = 0;
        virtual void visitCharacterLiteral(CharacterLiteral* expr) = 0;
        virtual void visitArrayLiteral(ArrayLiteral* expr) = 0;
    };

    // Types
    class FunctionType;
    class PrimitiveType;
    class PointerType;
    class SimpleType;

    class TypeVisitor {
    public:
        // Types
        virtual void visitFunctionType(FunctionType* type) = 0;
        virtual void visitPrimitiveType(PrimitiveType* type) = 0;
        virtual void visitPointerType(PointerType* type) = 0;
        virtual void visitSimpleType(SimpleType* type) = 0;
    };
}