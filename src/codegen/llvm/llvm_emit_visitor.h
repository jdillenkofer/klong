#pragma once

#include <deque>

#include "common/compilation_session.h"
#include "ast/stmt.h"
#include "ast/visitor.h"

#include "llvm_type_emit_visitor.h"
#include "llvm_debug_type_emit_visitor.h"

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/LLVMContext.h"

namespace klong {

    class LLVMEmitVisitor : public StmtVisitor, public ExprVisitor {
    public:
        explicit LLVMEmitVisitor(const llvm::DataLayout dataLayout);

        llvm::Module* getModule();

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
        void visitStringLiteral(StringLiteral* expr) override;
        void visitCharacterLiteral(CharacterLiteral* expr) override;
        void visitArrayLiteral(ArrayLiteral* expr) override;

        void setSession(CompilationSession* session);

    private:
        llvm::Value* emitCodeL(Expr* expr);
        llvm::Value* emitCodeR(Expr* expr);
        void emitCode(Stmt *stmt);
        void emitBlock(const std::vector<Stmt*>& statements);
        void emitLocalDefers();
        void emitAllDefers();
        llvm::Value* getVariableAddress(Expr* expr);
        llvm::Value* emitCast(llvm::Value *value, Type *from, Type *to);
		std::shared_ptr<ExternalDeclaration> translateToExternalDeclaration(Stmt* stmt);

		void emitDebugLocation(Stmt* stmt);
		llvm::DIScope* getDebugScope();
    private:
        llvm::LLVMContext _context;

        llvm::IRBuilder<> _builder;

		// DEBUG INFO
		llvm::DIFile* _debugFile;
		llvm::DICompileUnit* _debugCompilationUnit;
		std::unique_ptr<llvm::DIBuilder> _debugInfoBuilder;
		std::vector<llvm::DIScope*> _debugBlocks;
        
		LLVMTypeEmitVisitor _typeEmitVisitor;
		LLVMDebugTypeEmitVisitor _debugTypeEmitVisitor;

        std::unique_ptr<llvm::Module> _module;

        bool _isCodeL = false;
        llvm::Value* _valueOfLastExpr = nullptr;
        std::map<Stmt*, llvm::Value*> _namedValues;

        // used as jmp targets for break and continue
        llvm::BasicBlock* _breakJmpTarget = nullptr;
        llvm::BasicBlock* _continueJmpTarget = nullptr;

        // used for dead code elimination after terminating stmt in block
		llvm::BasicBlock* _previousBlock = nullptr;
		bool _eliminateDeadCodeInCurrentBlock;

		// used for defer
		std::deque<std::vector<Stmt*>> _deferScopes;
		CompilationSession* _session;
    };
}