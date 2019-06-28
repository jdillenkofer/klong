#pragma once

#include "common/compilation_session.h"

namespace klong {
    class CEmitVisitor : public StmtVisitor, public ExprVisitor, public TypeVisitor {
    public:
        void setSession(CompilationSession* session);

        virtual void visitModule(Module * module) override;
        virtual void visitBlockStmt(Block * stmt) override;
        virtual void visitExpressionStmt(Expression * stmt) override;
        virtual void visitExtDeclStmt(ExternalDeclaration * stmt) override;
        virtual void visitImportStmt(Import * stmt) override;
        virtual void visitFunctionStmt(Function * stmt) override;
        virtual void visitParameterStmt(Parameter * stmt) override;
        virtual void visitIfStmt(If * stmt) override;
        virtual void visitReturnStmt(Return * stmt) override;
        virtual void visitVarDeclStmt(VariableDeclaration * stmt) override;
        virtual void visitStructDeclStmt(StructDeclaration * stmt) override;
        virtual void visitUnionDeclStmt(UnionDeclaration * stmt) override;
        virtual void visitEnumDeclStmt(EnumDeclaration * stmt) override;
        virtual void visitCustomMemberStmt(CustomMember * stmt) override;
        virtual void visitWhileStmt(While * stmt) override;
        virtual void visitForStmt(For * stmt) override;
        virtual void visitBreakStmt(Break * stmt) override;
        virtual void visitContinueStmt(Continue * stmt) override;
        virtual void visitDeferStmt(Defer * stmt) override;
        virtual void visitCommentStmt(Comment * expr) override;

        virtual void visitAssignExpr(Assign * expr) override;
        virtual void visitBinaryExpr(Binary * expr) override;
        virtual void visitCallExpr(Call * expr) override;
        virtual void visitGroupingExpr(Grouping * expr) override;
        virtual void visitSubscriptExpr(Subscript * expr) override;
        virtual void visitMemberAccessExpr(MemberAccess * expr) override;
        virtual void visitEnumAccessExpr(EnumAccess * expr) override;
        virtual void visitLogicalExpr(Logical * expr) override;
        virtual void visitUnaryExpr(Unary * expr) override;
        virtual void visitSizeOfExpr(SizeOf * expr) override;
        virtual void visitCastExpr(Cast * expr) override;
        virtual void visitVariableExpr(Variable * expr) override;
        virtual void visitNumberLiteral(NumberLiteral * expr) override;
        virtual void visitBoolLiteral(BoolLiteral * expr) override;
        virtual void visitNullLiteral(NullLiteral * expr) override;
        virtual void visitStringLiteral(StringLiteral * expr) override;
        virtual void visitCharacterLiteral(CharacterLiteral * expr) override;
        virtual void visitArrayLiteral(ArrayLiteral * expr) override;

        virtual void visitFunctionType(FunctionType * type) override;
        virtual void visitPrimitiveType(PrimitiveType * type) override;
        virtual void visitPointerType(PointerType * type) override;
        virtual void visitCustomType(CustomType * type) override;

        std::string getOutput() const;
    private:
        CompilationSession* _session;
        std::stringstream _outputStream;
    };
}