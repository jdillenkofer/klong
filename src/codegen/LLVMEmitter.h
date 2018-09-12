#pragma once

#include <parser/stmt.h>
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "../parser/visitor.h"

namespace klong {

    static llvm::LLVMContext context;
    static llvm::IRBuilder<> IRBuilder(context);

    class LLVMEmitter : public Visitor {
        public:
            LLVMEmitter() = default;

            // Module
            void visitModule(Module* module) override;

            // Stmt
            void visitBlockStmt(Block* stmt) override;
            void visitExpressionStmt(Expression* stmt) override;
            void visitFunctionStmt(Function* stmt) override;
            void visitParameterStmt(Parameter* stmt) override;
            void visitIfStmt(If* stmt) override;
            void visitPrintStmt(Print* stmt) override;
            void visitReturnStmt(Return* stmt) override;
            void visitLetStmt(Let* stmt) override;
            void visitConstStmt(Const* stmt) override;
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

            llvm::Module* module() const {
                return _module.get();
            }

        private:
            llvm::Value* emit(Expr* expr);
            void emit(Stmt* stmt);
            void emitBlock(const std::vector<StmtPtr>& statements);
        private:
            std::unique_ptr<llvm::Module> _module;
            llvm::Value* _valueOfLastExpr;
            llvm::BasicBlock* _currentBlock;
    };
}