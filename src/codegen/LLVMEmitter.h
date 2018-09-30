#pragma once

#include "../parser/stmt.h"
#include "../parser/visitor.h"

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"

namespace klong {

    static llvm::LLVMContext context;
    static llvm::IRBuilder<> IRBuilder(context);

    class LLVMEmitter : public Visitor {
    public:
        LLVMEmitter();

        // Module
        void visitModule(Module* module) override;

        // Stmt
        void visitBlockStmt(Block* stmt) override;
        void visitExpressionStmt(Expression* stmt) override;
        void visitExtDeclStmt(ExternalDeclaration* stmt) override;
        void visitFunctionStmt(Function* stmt) override;
        void visitParameterStmt(Parameter* stmt) override;
        void visitIfStmt(If* stmt) override;
        void visitPrintStmt(Print* stmt) override;
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
        void visitVariableExpr(Variable* expr) override;

        // Literals
        void visitNumberLiteral(NumberLiteral* expr) override;
        void visitBoolLiteral(BoolLiteral* expr) override;
        void visitStringLiteral(StringLiteral* expr) override;
        void visitCharacterLiteral(CharacterLiteral* expr) override;

        // Types
        void visitFunctionType(FunctionType* type) override;
        void visitPrimitiveType(PrimitiveType* type) override;
        void visitPointerType(PointerType* type) override;
        void visitSimpleType(SimpleType *type) override;

        void printIR();
        bool generateObjectFile(std::string outputFilename,
                                std::string targetTriple = llvm::sys::getDefaultTargetTriple(),
                                std::string cpu = "generic",
                                std::string features = "");

    private:
        llvm::Value* emit(Expr* expr);
        llvm::Value* emit(Stmt* stmt);
        void emitBlock(const std::vector<StmtPtr>& statements);
    private:
        std::unique_ptr<llvm::Module> _module;
        llvm::Value* _valueOfLastExpr = nullptr;
        llvm::Type* _valueOfLastType = nullptr;
        std::map<Stmt*, llvm::Value*> _namedValues;
        TypeKind _outerType = TypeKind::PRIMITIVE;
        static bool _initialized;
    };
}