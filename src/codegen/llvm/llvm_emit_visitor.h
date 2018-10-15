#pragma once

#include "ast/stmt.h"
#include "ast/visitor.h"

#include "llvm_type_emit_visitor.h"

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"

namespace klong {

    class LLVMEmitVisitor : public StmtVisitor, public ExprVisitor {
    public:
        LLVMEmitVisitor();

        llvm::Module* getModule();

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
        void visitCastExpr(Cast* expr) override;
        void visitVariableExpr(Variable* expr) override;

        // Literals
        void visitNumberLiteral(NumberLiteral* expr) override;
        void visitBoolLiteral(BoolLiteral* expr) override;
        void visitStringLiteral(StringLiteral* expr) override;
        void visitCharacterLiteral(CharacterLiteral* expr) override;

    private:
        llvm::Value* emit(Expr* expr);
        void emit(Stmt* stmt);
        void emitBlock(const std::vector<Stmt*>& statements);
        llvm::Value* getVariableAddress(Expr* expr);
        llvm::Value* emitCast(llvm::Value *value, Type *from, Type *to);
    private:
        llvm::LLVMContext _context;
        llvm::IRBuilder<> _builder;
        LLVMTypeEmitVisitor _typeEmitVisitor;

        std::unique_ptr<llvm::Module> _module;

        llvm::Value* _valueOfLastExpr = nullptr;
        std::map<Stmt*, llvm::Value*> _namedValues;

        // used for dead code elimination after return stmt
        llvm::BasicBlock* _previousBlock = nullptr;
        bool _eliminateDeadCodeInCurrentBlock;
    };
}