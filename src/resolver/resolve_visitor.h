#pragma once

#include <map>

#include "array.h"
#include "common/compilation_session.h"
#include "common/symbol_info.h"
#include "ast/module.h"
#include "ast/visitor.h"
#include "ast/stmt.h"
#include "ast/expr.h"

namespace klong {
    class ResolveException : public std::exception {
    public:
        ResolveException(SourceRange sourceRange, std::string message):
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

    class ResolveVisitor : public StmtVisitor, public ExprVisitor {
    public:
        explicit ResolveVisitor(CompilationSession* session):
            _session(session) {

        }

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
        void visitCommentStmt(Comment* expr) override;

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

    private:
        void resolve(Stmt* stmt);
        void resolve(Expr* expr);
        void resolve(const Array<Stmt*>& statements);

        bool resolveLocal(Variable* variable);

        void resolveFunction(Function* stmt);

        void enterScope();
        void exitScope();

        void declare(Stmt* declarationStmt, std::string name, DeclarationType declarationType, bool isPublic = false);
        void define(std::string name);

    private:
        Module* _module;
        Array<std::map<std::string, SymbolInfo>> _scopes;
        bool _isInsideFunction = false;
        CompilationSession* _session;
    };
}