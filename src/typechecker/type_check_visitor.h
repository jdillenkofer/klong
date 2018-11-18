#pragma once

#include <map>
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

    class TypeCheckVisitor : public StmtVisitor, public ExprVisitor, public TypeVisitor {
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
        void visitStringLiteral(StringLiteral* expr) override;
        void visitCharacterLiteral(CharacterLiteral* expr) override;
        void visitArrayLiteral(ArrayLiteral* expr) override;

        // Types
        void visitFunctionType(FunctionType* type);
        void visitPrimitiveType(PrimitiveType* type);
        void visitPointerType(PointerType* type);
        void visitCustomType(CustomType *type);

        Result<ModulePtr, TypeCheckException> getResult() const;

    private:
        void check(const std::vector<Stmt*>& statements);
        void check(Stmt* stmt);
        void check(Expr* expr);

        void resolveType(Type* type);
        void declareType(TypeDeclaration* typeDeclarationStmt);
        TypeDeclaration* findTypeDeclaration(CustomType* type);

		void checkMemberTypeDeclStmt(MemberTypeDeclaration* stmt);
        bool getAndResetReturnsValue();
        TypePtr applyIntegerPromotion(Type* type);
        TypePtr applyArithmeticPromotion(Type* left, Type* right);
    private:
        Function* currentFunction = nullptr;
        std::map<std::string, TypeDeclaration*> _typeDeclarations;
        Result<ModulePtr, TypeCheckException> _result;
        bool _returnsValue = false;
        std::vector<TypePtr> _arithmeticConversionStack = {
                std::make_shared<PrimitiveType>(PrimitiveTypeKind::I8),
                std::make_shared<PrimitiveType>(PrimitiveTypeKind::U8),
                std::make_shared<PrimitiveType>(PrimitiveTypeKind::I16),
                std::make_shared<PrimitiveType>(PrimitiveTypeKind::U16),
                std::make_shared<PrimitiveType>(PrimitiveTypeKind::I32),
                std::make_shared<PrimitiveType>(PrimitiveTypeKind::U32),
                std::make_shared<PrimitiveType>(PrimitiveTypeKind::I64),
                std::make_shared<PrimitiveType>(PrimitiveTypeKind::U64),
                std::make_shared<PrimitiveType>(PrimitiveTypeKind::F32),
                std::make_shared<PrimitiveType>(PrimitiveTypeKind::F64)
        };
    };
}