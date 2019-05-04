#pragma once

#include "ast/stmt.h"
#include "ast/visitor.h"

#include <map>

namespace klong {
    class DotfileVisitor : public StmtVisitor, public ExprVisitor, public TypeVisitor {
    public:
        // Module
        void visitModule(Module* module) override;

        // Stmt
        void visitBlockStmt(Block* stmt) override;
        void visitExpressionStmt(Expression* stmt) override;
        void visitExtDeclStmt(ExternalDeclaration* stmt) override;
        void visitImportStmt(Import* stmt) override;
        void visitFunctionStmt(Function* stmt) override;
        void visitParameterStmt(Parameter* stmt) override;
        void visitIfStmt(If* stmt) override;
        void visitReturnStmt(Return* stmt) override;
        void visitVarDeclStmt(VariableDeclaration* stmt) override;
		void visitStructDeclStmt(StructDeclaration* stmt) override;
        void visitUnionDeclStmt(UnionDeclaration* stmt) override;
		void visitEnumDeclStmt(EnumDeclaration* stmt) override;
		void visitCustomMemberStmt(CustomMember* stmt) override;
        void visitWhileStmt(While* stmt) override;
        void visitForStmt(For* stmt) override;
        void visitBreakStmt(Break* stmt) override;
        void visitContinueStmt(Continue* stmt) override;
        void visitDeferStmt(Defer* stmt) override;
        void visitCommentStmt(Comment* stmt) override;

        // Expr
        void visitAssignExpr(Assign* expr) override;
        void visitBinaryExpr(Binary* expr) override;
        void visitCallExpr(Call* expr) override;
        void visitGroupingExpr(Grouping* expr) override;
		void visitSubscriptExpr(Subscript* expr) override;
		void visitMemberAccessExpr(MemberAccess* expr) override;
		void visitEnumAccessExpr(EnumAccess* expr) override;
        void visitLogicalExpr(Logical* expr) override;
        void visitUnaryExpr(Unary* expr) override;
        void visitSizeOfExpr(SizeOf* expr) override;
        void visitCastExpr(Cast* expr) override;
        void visitVariableExpr(Variable* expr) override;

        // Literals
        void visitNumberLiteral(NumberLiteral* expr) override;
        void visitBoolLiteral(BoolLiteral* expr) override;
        void visitNullLiteral(NullLiteral* expr) override;
        void visitStringLiteral(StringLiteral* expr) override;
        void visitCharacterLiteral(CharacterLiteral* expr) override;
        void visitArrayLiteral(ArrayLiteral* expr) override;

        // Types
        void visitFunctionType(FunctionType* type) override;
        void visitPrimitiveType(PrimitiveType *type) override;
        void visitPointerType(PointerType *type) override;
        void visitCustomType(CustomType *type) override;

        const std::string& getDotfileOutput() {
            return _output;
        }

    private:
        void reset();

        uint64_t getModuleId(Module* module);
        uint64_t getStmtId(Stmt* stmt);
        uint64_t getExprId(Expr* expr);

        void appendLine(const std::string& append);
        std::string getType(Type* type);

    private:
        uint64_t _counter = 0;
        std::map<Module*, uint64_t> _moduleToId;
        std::map<Stmt*, uint64_t> _stmtToId;
        std::map<Expr*, uint64_t> _exprToId;

        std::string _typeOfLastExpr;

        std::string _output;
    };
}