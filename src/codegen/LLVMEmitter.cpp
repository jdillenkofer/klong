#include "LLVMEmitter.h"

#include "../parser/module.h"
#include "../parser/stmt.h"
#include "../parser/expr.h"

namespace klong {

    llvm::Value* LLVMEmitter::emit(Expr* expr) {
        expr->accept(this);
        return _valueOfLastExpr;
    }

    void LLVMEmitter::emit(Stmt* stmt) {
        stmt->accept(this);
    }

    void LLVMEmitter::emitBlock(const std::vector<StmtPtr>& statements) {
        llvm::BasicBlock* previous = _currentBlock;
        _currentBlock = llvm::BasicBlock::Create(context);
        IRBuilder.SetInsertPoint(_currentBlock);

        for (auto& stmt : statements) {
            stmt->accept(this);
        }

        _currentBlock = previous;
        IRBuilder.SetInsertPoint(_currentBlock);
    }

    void LLVMEmitter::visitModule(Module* module) {
        _module = llvm::make_unique<llvm::Module>(module->filename(), context);
        emitBlock(module->statements());
    }

    void LLVMEmitter::visitBlockStmt(Block* stmt) {
        emitBlock(stmt->statements());
    }

    void LLVMEmitter::visitExpressionStmt(Expression* stmt) {
        emit(stmt->expression().get());
    }

    void LLVMEmitter::visitFunctionStmt(Function* stmt) {
        // check for an existing function
        llvm::Function* function = _module->getFunction(stmt->name());

    }

    void LLVMEmitter::visitParameterStmt(Parameter* stmt) {

    }

    void LLVMEmitter::visitIfStmt(If* stmt) {

    }

    void LLVMEmitter::visitPrintStmt(Print* stmt) {

    }

    void LLVMEmitter::visitReturnStmt(Return* stmt) {

    }

    void LLVMEmitter::visitLetStmt(Let* stmt) {

    }

    void LLVMEmitter::visitConstStmt(Const* stmt) {

    }

    void LLVMEmitter::visitWhileStmt(While* stmt) {

    }

    void LLVMEmitter::visitForStmt(For* stmt) {

    }

    void LLVMEmitter::visitCommentStmt(Comment* stmt) {
        (void) stmt;
    }

    void LLVMEmitter::visitAssignExpr(Assign* expr) {

    }

    void LLVMEmitter::visitBinaryExpr(Binary* expr) {
        llvm::Value* left = emit(expr->left().get());
        llvm::Value* right = emit(expr->right().get());
        PrimitiveType* type = dynamic_cast<PrimitiveType*>(expr->left()->type().get());
        if (type == nullptr) {
            // TODO: exception handling
            return;
        }
        if (type->isInteger()) {
            switch (expr->op()) {
                case BinaryOperation::PLUS:
                    _valueOfLastExpr = IRBuilder.CreateAdd(left, right);
                    break;
                case BinaryOperation::MINUS:
                    _valueOfLastExpr = IRBuilder.CreateSub(left, right);
                    break;
                case BinaryOperation::MULTIPLICATION:
                    _valueOfLastExpr = IRBuilder.CreateMul(left, right);
                    break;
                case BinaryOperation::DIVISION:
                    if (type->isSigned()) {
                        _valueOfLastExpr = IRBuilder.CreateSDiv(left, right);
                    } else {
                        _valueOfLastExpr = IRBuilder.CreateUDiv(left, right);
                    }
                    break;
                case BinaryOperation::EQUALITY:
                    _valueOfLastExpr = IRBuilder.CreateICmpEQ(left, right);
                    break;
                case BinaryOperation::INEQUALITY:
                    _valueOfLastExpr = IRBuilder.CreateICmpNE(left, right);
                    break;
                case BinaryOperation::LESS_THAN:
                    if (type->isSigned()) {
                        _valueOfLastExpr = IRBuilder.CreateICmpSLT(left, right);
                    } else {
                        _valueOfLastExpr = IRBuilder.CreateICmpULT(left, right);
                    }
                    break;
                case BinaryOperation::LESS_EQUAL:
                    if (type->isSigned()) {
                        _valueOfLastExpr = IRBuilder.CreateICmpSLE(left, right);
                    } else {
                        _valueOfLastExpr = IRBuilder.CreateICmpULE(left, right);
                    }
                    break;
                case BinaryOperation::GREATER_THAN:
                    if (type->isSigned()) {
                        _valueOfLastExpr = IRBuilder.CreateICmpSGT(left, right);
                    } else {
                        _valueOfLastExpr = IRBuilder.CreateICmpUGT(left, right);
                    }
                    break;
                case BinaryOperation::GREATER_EQUAL:
                    if (type->isSigned()) {
                        _valueOfLastExpr = IRBuilder.CreateICmpSGE(left, right);
                    } else {
                        _valueOfLastExpr = IRBuilder.CreateICmpUGE(left, right);
                    }
                    break;
                default:
                    break;
            }
            return;
        }
        if (type->isFloat()) {
            switch (expr->op()) {
                case BinaryOperation::PLUS:
                    _valueOfLastExpr = IRBuilder.CreateFAdd(left, right);
                    break;
                case BinaryOperation::MINUS:
                    _valueOfLastExpr = IRBuilder.CreateFSub(left, right);
                    break;
                case BinaryOperation::MULTIPLICATION:
                    _valueOfLastExpr = IRBuilder.CreateFMul(left, right);
                    break;
                case BinaryOperation::DIVISION:
                    _valueOfLastExpr = IRBuilder.CreateFDiv(left, right);
                    break;
                case BinaryOperation::EQUALITY:
                    _valueOfLastExpr = IRBuilder.CreateFCmpUEQ(left, right);
                    break;
                case BinaryOperation::INEQUALITY:
                    _valueOfLastExpr = IRBuilder.CreateFCmpUNE(left, right);
                    break;
                case BinaryOperation::LESS_THAN:
                    _valueOfLastExpr = IRBuilder.CreateFCmpULT(left, right);
                    break;
                case BinaryOperation::LESS_EQUAL:
                    _valueOfLastExpr = IRBuilder.CreateFCmpULE(left, right);
                    break;
                case BinaryOperation::GREATER_THAN:
                    _valueOfLastExpr = IRBuilder.CreateFCmpUGT(left, right);
                    break;
                case BinaryOperation::GREATER_EQUAL:
                    _valueOfLastExpr = IRBuilder.CreateFCmpUGE(left, right);
                    break;
                default:
                    break;
            }
            return;
        }
    }

    void LLVMEmitter::visitCallExpr(Call* expr) {
        Variable* functionVar = dynamic_cast<Variable*>(expr->callee().get());
        llvm::Function* calleeF = _module->getFunction(functionVar->name());
        if (calleeF == nullptr) {
            // TODO: exception handling
            return;
        }
        std::vector<llvm::Value*> argsV;
        for (auto& arg : expr->args()) {
            argsV.push_back(emit(arg.get()));
        }
        _valueOfLastExpr = IRBuilder.CreateCall(calleeF, argsV, "calltmp");
    }

    void LLVMEmitter::visitGroupingExpr(Grouping* expr) {
        _valueOfLastExpr = emit(expr->expression().get());
    }

    void LLVMEmitter::visitLogicalExpr(Logical* expr) {
        llvm::Value* left = emit(expr->left().get());
        llvm::Value* right = emit(expr->right().get());
        switch (expr->op()) {
            case LogicalOperation::AND:
                _valueOfLastExpr = IRBuilder.CreateAnd(left, right);
                break;
            case LogicalOperation::OR:
                _valueOfLastExpr = IRBuilder.CreateOr(left, right);
                break;
        }
    }

    void LLVMEmitter::visitUnaryExpr(Unary* expr) {
        llvm::Value* right = emit(expr->right().get());
        switch(expr->op()) {
            case UnaryOperation::MINUS:
                _valueOfLastExpr = IRBuilder.CreateNeg(right);
                break;
            case UnaryOperation::NOT:
                _valueOfLastExpr = IRBuilder.CreateNot(right);
                break;
        }
    }

    void LLVMEmitter::visitVariableExpr(Variable* expr) {

    }

    void LLVMEmitter::visitNumberLiteral(NumberLiteral* expr) {
        switch (expr->literalType()) {
            case PrimitiveTypeKind::I64:
                _valueOfLastExpr = llvm::ConstantInt::get(context, llvm::APInt(64, (uint64_t) expr->i64(), true));
                break;
            case PrimitiveTypeKind::U64:
                _valueOfLastExpr = llvm::ConstantInt::get(context, llvm::APInt(64, expr->u64(), false));
                break;
            case PrimitiveTypeKind::F64:
                _valueOfLastExpr = llvm::ConstantFP::get(context, llvm::APFloat(expr->f64()));
                break;
            default:
                // TODO: Error handling
                break;
        }
    }

    void LLVMEmitter::visitBoolLiteral(BoolLiteral* expr) {
        switch (expr->literalType()) {
            case PrimitiveTypeKind::BOOL:
                _valueOfLastExpr = llvm::ConstantInt::get(context, llvm::APInt(1, (uint64_t) expr->value()));
                break;
            default:
                // TODO: Error handling
                break;
        }
    }

    void LLVMEmitter::visitStringLiteral(StringLiteral* expr) {
        switch (expr->literalType()) {
            case PrimitiveTypeKind::STRING:
                _valueOfLastExpr = llvm::ConstantDataArray::getString(context, expr->value(), true);
                break;
            default:
                // TODO: Error handling
                break;
        }
    }

    void LLVMEmitter::visitCharacterLiteral(CharacterLiteral* expr) {
        switch (expr->literalType()) {
            case PrimitiveTypeKind::I8:
                _valueOfLastExpr = llvm::ConstantInt::get(context, llvm::APInt(8, (uint32_t) expr->value()));
                break;
            default:
                // TODO: Error handling
                break;
        }
    }

    void LLVMEmitter::visitFunctionType(FunctionType* type) {

    }

    void LLVMEmitter::visitPrimitiveType(PrimitiveType *type) {

    }

    void LLVMEmitter::visitSimpleType(SimpleType *type) {

    }
}