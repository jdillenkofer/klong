#include "llvm_emit_visitor.h"

#include "ast/module.h"
#include "ast/stmt.h"
#include "ast/expr.h"

#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/InstrTypes.h"

#include "llvm/Support/TargetSelect.h"

namespace klong {

    LLVMEmitVisitor::LLVMEmitVisitor() :
        _context(),
        _builder(_context),
        _typeEmitVisitor(_context) {

    }

    llvm::Module* LLVMEmitVisitor::getModule() {
        return _module.get();
    }

    llvm::Value* LLVMEmitVisitor::emit(Expr* expr) {
        expr->accept(this);
        return _valueOfLastExpr;
    }

    void LLVMEmitVisitor::emit(Stmt* stmt) {
        // eliminate dead code after return
		bool blockChanged = _builder.GetInsertBlock() != _previousBlock;
		if (blockChanged) {
			_previousBlock = _builder.GetInsertBlock();
			_eliminateDeadCodeInCurrentBlock = false;
		}
		if (_eliminateDeadCodeInCurrentBlock) {
			return;
		}
        stmt->accept(this);
    }

    void LLVMEmitVisitor::emitBlock(const std::vector<Stmt*>& statements) {
        for (auto& stmt : statements) {
            emit(stmt);
        }
    }

    llvm::Value* LLVMEmitVisitor::emitCast(llvm::Value *value, Type *from, Type *to) {
        auto targetType = _typeEmitVisitor.getLLVMType(to);
        auto myPrimSourceType = dynamic_cast<PrimitiveType*>(from);
        auto myPrimTargetType = dynamic_cast<PrimitiveType*>(to);

        auto myPointerSourceType = dynamic_cast<PointerType*>(from);
        auto myPointerTargetType = dynamic_cast<PointerType*>(to);

        if (myPrimSourceType && myPrimTargetType) {
            if ((myPrimSourceType->isInteger() || myPrimSourceType->isBoolean()) &&
                (myPrimTargetType->isInteger() || myPrimTargetType->isBoolean())) {
                return _builder.CreateIntCast(value, targetType, myPrimTargetType->isSigned());
            }

            if (myPrimSourceType->isFloat() && myPrimTargetType->isFloat()) {
                return _builder.CreateFPCast(value, targetType);
            }

            if ((myPrimSourceType->isInteger() || myPrimSourceType->isBoolean()) && (myPrimTargetType->isFloat())) {
                if (myPrimSourceType->isSigned()) {
                    return _builder.CreateSIToFP(value, targetType);
                } else {
                    return _builder.CreateUIToFP(value, targetType);
                }
            }

            if ((myPrimSourceType->isFloat()) || (myPrimTargetType->isInteger() || myPrimTargetType->isBoolean())) {
                if (myPrimTargetType->isSigned()) {
                    return _builder.CreateFPToSI(value, targetType);
                } else {
                    return _builder.CreateFPToUI(value, targetType);
                }
            }
        }

        if (myPrimSourceType && myPrimSourceType->isFloat() && myPointerTargetType) {
            auto valueAsU64 = _builder.CreateFPToUI(value, llvm::IntegerType::getInt64Ty(_context));
            return _builder.CreateIntToPtr(valueAsU64, targetType);
        }

        if (myPointerSourceType && myPrimTargetType && myPrimTargetType->isFloat()) {
            auto valueAsU64 = _builder.CreatePtrToInt(value, llvm::IntegerType::getInt64Ty(_context));
            return _builder.CreateUIToFP(valueAsU64, targetType);
        }

        if (myPointerSourceType && myPointerTargetType) {
            return _builder.CreatePointerCast(value, targetType);
        }
        if (myPrimSourceType && myPrimSourceType->isInteger() && myPointerTargetType) {
            return _builder.CreateIntToPtr(value, targetType);
        }
        if (myPointerSourceType && myPrimTargetType && myPrimTargetType->isInteger()) {
            return _builder.CreatePtrToInt(value, targetType);
        }

        // Type conversion failed
        assert(false);
        return value;
    }

    void LLVMEmitVisitor::visitModule(Module* module) {
        _module = llvm::make_unique<llvm::Module>(module->filename(), _context);

        for (auto& stmt : module->statements()) {
            if (stmt->kind() == StatementKind::FUNCTION) {
                auto function = dynamic_cast<Function*>(stmt);
                auto linkage = function->isPublic() ?
                        llvm::Function::ExternalLinkage : llvm::Function::InternalLinkage;

                auto functionType = (llvm::FunctionType*) _typeEmitVisitor.getLLVMType(function->functionType());

                auto llvmFunction = llvm::Function::Create(functionType,
                        linkage, function->name(), _module.get());

                _namedValues[stmt] = llvmFunction;
            }
        }

        for (auto& stmt : module->statements()) {
            stmt->accept(this);
        }
    }

    void LLVMEmitVisitor::visitBlockStmt(Block* stmt) {
        emitBlock(stmt->statements());
    }

    void LLVMEmitVisitor::visitExpressionStmt(Expression* stmt) {
        _valueOfLastExpr = emit(stmt->expression());
    }

    void LLVMEmitVisitor::visitExtDeclStmt(ExternalDeclaration* stmt) {
        auto type = _typeEmitVisitor.getLLVMType(stmt->type());

        bool isFunction = stmt->type()->kind() == TypeKind::FUNCTION;
        bool isPointer = stmt->type()->kind() == TypeKind::POINTER;
        if (isPointer) {
            auto pointerType = dynamic_cast<PointerType*>(stmt->type());
            auto pointsToType = pointerType->pointsTo();
            if (pointsToType->kind() == TypeKind::FUNCTION) {
                isFunction = true;
                type = _typeEmitVisitor.getLLVMType(pointsToType);
            }
        }

        if (isFunction) {
            llvm::Function::Create((llvm::FunctionType*) type, llvm::Function::ExternalLinkage, stmt->name(), _module.get());
            _namedValues[stmt] = _module->getFunction(stmt->name());
        } else {
            _namedValues[stmt] = _module->getOrInsertGlobal(stmt->name(), type);
        }
    }

    void LLVMEmitVisitor::visitFunctionStmt(Function* stmt) {
        llvm::Function* function = _module->getFunction(stmt->name());

        // Function body
        llvm::BasicBlock* bb = llvm::BasicBlock::Create(_context, "entry", function);
        _builder.SetInsertPoint(bb);

        {
            size_t i = 0;
            for (auto& arg : function->args()) {
                llvm::Type* paramType = _typeEmitVisitor.getLLVMType(stmt->params()[i]->type());
                auto param = _builder.CreateAlloca(paramType);
                _builder.CreateStore(&arg, param);
                _namedValues[stmt->params()[i]] = param;
                i++;
            }
        }

        emitBlock(stmt->body());
        llvm::verifyFunction(*function);
    }

    void LLVMEmitVisitor::visitParameterStmt(Parameter* stmt) {
        (void) stmt;
    }

    void LLVMEmitVisitor::visitIfStmt(If* stmt) {
        auto function = _builder.GetInsertBlock()->getParent();
        
		auto thenBB = llvm::BasicBlock::Create(_context, "thenBranch", function);
        auto elseBB = llvm::BasicBlock::Create(_context, "elseBranch");
        auto mergeBB = llvm::BasicBlock::Create(_context, "mergeBranch");

		auto condV = emit(stmt->condition());
        _builder.CreateCondBr(condV, thenBB, elseBB);

        _builder.SetInsertPoint(thenBB);
        emit(stmt->thenBranch());
		if (!_eliminateDeadCodeInCurrentBlock) {
			_builder.CreateBr(mergeBB);
		}

		function->getBasicBlockList().push_back(elseBB);
        _builder.SetInsertPoint(elseBB);
        if (stmt->elseBranch() != nullptr) {
            emit(stmt->elseBranch());
		} else {
			// since no else branch exists we can be sure that, if the thenBranch is not taken,
			// the next part of the code can still get executed
			_eliminateDeadCodeInCurrentBlock = false;
		}

		if (!_eliminateDeadCodeInCurrentBlock) {
			_builder.CreateBr(mergeBB);
		}

		function->getBasicBlockList().push_back(mergeBB);
        _builder.SetInsertPoint(mergeBB);
        if (stmt->isMergeUnreachable()) {
            _builder.CreateUnreachable();
        }
    }

    void LLVMEmitVisitor::visitReturnStmt(Return* stmt) {
        if (stmt->value() != nullptr) {
            emit(stmt->value());
        }
        _builder.CreateRet(_valueOfLastExpr);
		_eliminateDeadCodeInCurrentBlock = true;
    }

    void LLVMEmitVisitor::visitVarDeclStmt(VariableDeclaration* stmt) {
        auto type = _typeEmitVisitor.getLLVMType(stmt->type());

        if (stmt->isGlobal()) {
            _module->getOrInsertGlobal(stmt->name(), type);
            auto global = _module->getNamedGlobal(stmt->name());
            auto linkage = stmt->isPublic() ? llvm::GlobalValue::ExternalLinkage : llvm::GlobalValue::InternalLinkage;
            global->setLinkage(linkage);
            if (stmt->isConst()) {
                global->setConstant(true);
            }
            global->setInitializer((llvm::Constant*) emit(stmt->initializer()));
            _namedValues[stmt] = global;
        } else {
            auto stackPtr = _builder.CreateAlloca(type);
            _namedValues[stmt] = stackPtr;
            auto value = emit(stmt->initializer());
            _builder.CreateStore(value, stackPtr);
        }
    }

    void LLVMEmitVisitor::visitWhileStmt(While* stmt) {
        llvm::Function* function = _builder.GetInsertBlock()->getParent();

        auto whileCondBB = llvm::BasicBlock::Create(_context, "whileCond", function);
        auto whileBodyBB = llvm::BasicBlock::Create(_context, "whileBody");
        auto mergeWhileBB = llvm::BasicBlock::Create(_context, "mergeWhile");

        _builder.CreateBr(whileCondBB);

        _builder.SetInsertPoint(whileCondBB);
        llvm::Value* condV = emit(stmt->condition());
        _builder.CreateCondBr(condV, whileBodyBB, mergeWhileBB);

        function->getBasicBlockList().push_back(whileBodyBB);
        _builder.SetInsertPoint(whileBodyBB);

        auto prevBreakJmpTarget = _breakJmpTarget;
        auto prevContinueJmpTarget = _continueJmpTarget;
        _breakJmpTarget = mergeWhileBB;
        _continueJmpTarget = whileCondBB;

        emit(stmt->body());

        _continueJmpTarget = prevContinueJmpTarget;
        _breakJmpTarget = prevBreakJmpTarget;

		// we need to check here, if the body contains any terminators
        if (!_eliminateDeadCodeInCurrentBlock) {
            _builder.CreateBr(whileCondBB);
        }

        function->getBasicBlockList().push_back(mergeWhileBB);
        _builder.SetInsertPoint(mergeWhileBB);
    }

    void LLVMEmitVisitor::visitForStmt(For* stmt) {
        auto function = _builder.GetInsertBlock()->getParent();

        auto forInitBB = llvm::BasicBlock::Create(_context, "forInit", function);
        auto forCondBB = llvm::BasicBlock::Create(_context, "forCond", function);
        auto forBodyBB = llvm::BasicBlock::Create(_context, "forBody");
        auto forIncBB = llvm::BasicBlock::Create(_context, "forInc");
        auto mergeForBB = llvm::BasicBlock::Create(_context, "mergeFor");

        _builder.CreateBr(forInitBB);

        _builder.SetInsertPoint(forInitBB);
        if (stmt->initializer() != nullptr) {
            emit(stmt->initializer());
        }
        _builder.CreateBr(forCondBB);

        _builder.SetInsertPoint(forCondBB);
        llvm::Value* condV = emit(stmt->condition());
        _builder.CreateCondBr(condV, forBodyBB, mergeForBB);

        function->getBasicBlockList().push_back(forBodyBB);
        _builder.SetInsertPoint(forBodyBB);
        auto prevBreakJmpTarget = _breakJmpTarget;
        auto prevContinueJmpTarget = _continueJmpTarget;
        _breakJmpTarget = mergeForBB;
        _continueJmpTarget = forIncBB;

        emit(stmt->body());

        _continueJmpTarget = prevContinueJmpTarget;
        _breakJmpTarget = prevBreakJmpTarget;

		// we need to check here, if the body contains any terminators
		if (!_eliminateDeadCodeInCurrentBlock) {
            _builder.CreateBr(forIncBB);
        }

        function->getBasicBlockList().push_back(forIncBB);
        _builder.SetInsertPoint(forIncBB);
        if (stmt->increment() != nullptr) {
            emit(stmt->increment());
        }
        _builder.CreateBr(forCondBB);

        function->getBasicBlockList().push_back(mergeForBB);
        _builder.SetInsertPoint(mergeForBB);
    }

    void LLVMEmitVisitor::visitBreakStmt(Break* stmt) {
        (void) stmt;
        _builder.CreateBr(_breakJmpTarget);
		_eliminateDeadCodeInCurrentBlock = true;
    }

    void LLVMEmitVisitor::visitContinueStmt(Continue* stmt) {
        (void) stmt;
        _builder.CreateBr(_continueJmpTarget);
		_eliminateDeadCodeInCurrentBlock = true;
    }

    void LLVMEmitVisitor::visitCommentStmt(Comment* stmt) {
        (void) stmt;
    }

    void LLVMEmitVisitor::visitAssignExpr(Assign* expr) {
        auto value = emit(expr->value());
        llvm::Value* address = nullptr;
        if (expr->isTargetVariable()) {
            address = getVariableAddress(expr->target());
        } else {
            // ignore the deref operator and
            // just get the address of the variable
            address = emit(expr->targetDeref()->right());
        }
        _builder.CreateStore(value, address);
        _valueOfLastExpr = value;
    }

    void LLVMEmitVisitor::visitBinaryExpr(Binary* expr) {
        llvm::Value* left = emit(expr->left());
        llvm::Value* right = emit(expr->right());
        auto leftType = dynamic_cast<PrimitiveType*>(expr->left()->type());
        auto targetType = dynamic_cast<PrimitiveType*>(expr->type());
        if (targetType && targetType->isInteger()) {
            if (!expr->type()->isEqual(expr->left()->type())) {
                left = emitCast(left, expr->left()->type(), expr->type());
            }
            if (!expr->type()->isEqual(expr->right()->type())) {
                right = emitCast(right, expr->right()->type(), expr->type());
            }
            switch (expr->op()) {
                case BinaryOperation::PLUS:
                    _valueOfLastExpr = _builder.CreateAdd(left, right);
                    break;
                case BinaryOperation::MINUS:
                    _valueOfLastExpr = _builder.CreateSub(left, right);
                    break;
                case BinaryOperation::MULTIPLICATION:
                    _valueOfLastExpr = _builder.CreateMul(left, right);
                    break;
                case BinaryOperation::DIVISION:
                    if (targetType->isSigned()) {
                        _valueOfLastExpr = _builder.CreateSDiv(left, right);
                    } else {
                        _valueOfLastExpr = _builder.CreateUDiv(left, right);
                    }
                    break;
                case BinaryOperation::MODULO:
                    if (targetType->isSigned()) {
                        _valueOfLastExpr = _builder.CreateSRem(left, right);
                    } else {
                        _valueOfLastExpr = _builder.CreateURem(left, right);
                    }
                    break;
                case BinaryOperation::LSL:
                    _valueOfLastExpr = _builder.CreateShl(left, right);
                    break;
                case BinaryOperation::LSR:
                    _valueOfLastExpr = _builder.CreateLShr(left, right);
                    break;
                case BinaryOperation::ASR:
                    _valueOfLastExpr = _builder.CreateAShr(left, right);
                    break;
                case BinaryOperation::AND:
                    _valueOfLastExpr = _builder.CreateAnd(left, right);
                    break;
                case BinaryOperation::XOR:
                    _valueOfLastExpr = _builder.CreateXor(left, right);
                    break;
                case BinaryOperation::OR:
                    _valueOfLastExpr = _builder.CreateOr(left, right);
                    break;
                default:
                    assert(false);
                    break;
            }
            return;
        }
        if (targetType && targetType->isFloat()) {
            if (!expr->type()->isEqual(expr->left()->type())) {
                left = emitCast(left, expr->left()->type(), expr->type());
            }
            if (!expr->type()->isEqual(expr->right()->type())) {
                right = emitCast(right, expr->right()->type(), expr->type());
            }
            switch (expr->op()) {
                case BinaryOperation::PLUS:
                    _valueOfLastExpr = _builder.CreateFAdd(left, right);
                    break;
                case BinaryOperation::MINUS:
                    _valueOfLastExpr = _builder.CreateFSub(left, right);
                    break;
                case BinaryOperation::MULTIPLICATION:
                    _valueOfLastExpr = _builder.CreateFMul(left, right);
                    break;
                case BinaryOperation::DIVISION:
                    _valueOfLastExpr = _builder.CreateFDiv(left, right);
                    break;
                case BinaryOperation::MODULO:
                    _valueOfLastExpr = _builder.CreateFRem(left, right);
                    break;
                default:
                    assert(false);
                    break;
            }
            return;
        }
        if (leftType && targetType && leftType->isInteger() && targetType->isBoolean()) {
            if (expr->castToType()) {
                left = emitCast(left, expr->left()->type(), expr->castToType());
                right = emitCast(right, expr->right()->type(), expr->castToType());
            }
            switch (expr->op()) {
                case BinaryOperation::EQUALITY:
                    _valueOfLastExpr = _builder.CreateICmpEQ(left, right);
                    break;
                case BinaryOperation::INEQUALITY:
                    _valueOfLastExpr = _builder.CreateICmpNE(left, right);
                    break;
                case BinaryOperation::LESS_THAN:
                    if (leftType->isSigned()) {
                        _valueOfLastExpr = _builder.CreateICmpSLT(left, right);
                    } else {
                        _valueOfLastExpr = _builder.CreateICmpULT(left, right);
                    }
                    break;
                case BinaryOperation::LESS_EQUAL:
                    if (leftType->isSigned()) {
                        _valueOfLastExpr = _builder.CreateICmpSLE(left, right);
                    } else {
                        _valueOfLastExpr = _builder.CreateICmpULE(left, right);
                    }
                    break;
                case BinaryOperation::GREATER_THAN:
                    if (leftType->isSigned()) {
                        _valueOfLastExpr = _builder.CreateICmpSGT(left, right);
                    } else {
                        _valueOfLastExpr = _builder.CreateICmpUGT(left, right);
                    }
                    break;
                case BinaryOperation::GREATER_EQUAL:
                    if (leftType->isSigned()) {
                        _valueOfLastExpr = _builder.CreateICmpSGE(left, right);
                    } else {
                        _valueOfLastExpr = _builder.CreateICmpUGE(left, right);
                    }
                    break;
                default:
                    assert(false);
                    break;
            }
        }
        if (leftType && targetType && leftType->isFloat() && targetType->isBoolean()) {
            switch (expr->op()) {
                case BinaryOperation::EQUALITY:
                    _valueOfLastExpr = _builder.CreateFCmpUEQ(left, right);
                    break;
                case BinaryOperation::INEQUALITY:
                    _valueOfLastExpr = _builder.CreateFCmpUNE(left, right);
                    break;
                case BinaryOperation::LESS_THAN:
                    _valueOfLastExpr = _builder.CreateFCmpULT(left, right);
                    break;
                case BinaryOperation::LESS_EQUAL:
                    _valueOfLastExpr = _builder.CreateFCmpULE(left, right);
                    break;
                case BinaryOperation::GREATER_THAN:
                    _valueOfLastExpr = _builder.CreateFCmpUGT(left, right);
                    break;
                case BinaryOperation::GREATER_EQUAL:
                    _valueOfLastExpr = _builder.CreateFCmpUGE(left, right);
                    break;
                default:
                    assert(false);
                    break;
            }
        }
        auto leftPointerType = dynamic_cast<PointerType*>(expr->left()->type());
        auto rightType = dynamic_cast<PrimitiveType*>(expr->right()->type());
        if (leftPointerType && rightType && rightType->isInteger()) {
            switch (expr->op()) {
                case BinaryOperation::PLUS:
                    _valueOfLastExpr = _builder.CreateGEP(left, right);
                    break;
                case BinaryOperation::MINUS:
                    _valueOfLastExpr = _builder.CreateGEP(left, _builder.CreateNeg(right));
                    break;
                default:
                    assert(false);
                    break;
            }
        }
        auto rightPointerType = dynamic_cast<PointerType*>(expr->right()->type());
        if (leftPointerType && rightPointerType) {
            switch (expr->op()) {
                case BinaryOperation::EQUALITY:
                    _valueOfLastExpr = _builder.CreateICmpEQ(left, right);
                    break;
                case BinaryOperation::INEQUALITY:
                    _valueOfLastExpr = _builder.CreateICmpNE(left, right);
                    break;
                case BinaryOperation::LESS_THAN:
                    _valueOfLastExpr = _builder.CreateICmpULT(left, right);
                    break;
                case BinaryOperation::LESS_EQUAL:
                    _valueOfLastExpr = _builder.CreateICmpULE(left, right);
                    break;
                case BinaryOperation::GREATER_THAN:
                    _valueOfLastExpr = _builder.CreateICmpUGT(left, right);
                    break;
                case BinaryOperation::GREATER_EQUAL:
                    _valueOfLastExpr = _builder.CreateICmpUGE(left, right);
                    break;
                default:
                    assert(false);
                    break;
            }
        }
    }

    void LLVMEmitVisitor::visitCallExpr(Call* expr) {
        auto calleeF = emit(expr->callee());
        std::vector<llvm::Value*> argsV;
        for (auto& arg : expr->args()) {
            argsV.push_back(emit(arg));
        }
        _valueOfLastExpr = _builder.CreateCall(calleeF, argsV);
    }

    void LLVMEmitVisitor::visitGroupingExpr(Grouping* expr) {
        _valueOfLastExpr = emit(expr->expression());
    }

    void LLVMEmitVisitor::visitLogicalExpr(Logical* expr) {
        llvm::Value* left = emit(expr->left());
        llvm::Value* right = emit(expr->right());
        switch (expr->op()) {
            case LogicalOperation::AND:
                _valueOfLastExpr = _builder.CreateAnd(left, right);
                break;
            case LogicalOperation::OR:
                _valueOfLastExpr = _builder.CreateOr(left, right);
                break;
        }
    }

    void LLVMEmitVisitor::visitUnaryExpr(Unary* expr) {
        llvm::Value* right = emit(expr->right());
        switch(expr->op()) {
            case UnaryOperation::MINUS:
                _valueOfLastExpr = _builder.CreateNeg(right);
                break;
            case UnaryOperation::NOT:
                _valueOfLastExpr = _builder.CreateNot(right);
                break;
            case UnaryOperation::DEREF:
                _valueOfLastExpr = _builder.CreateLoad(right);
                break;
            case UnaryOperation::ADDRESS_OF:
                _valueOfLastExpr = getVariableAddress(expr->right());
                break;
        }
    }

    llvm::Value* LLVMEmitVisitor::getVariableAddress(Expr* expr) {
        auto variable = dynamic_cast<Variable*>(expr);
        assert(variable != nullptr);
        auto address = _namedValues[variable->resolvesTo()];
        return address;
    }

    void LLVMEmitVisitor::visitSizeOfExpr(SizeOf* expr) {
        auto pointerType = llvm::PointerType::get(_typeEmitVisitor.getLLVMType(expr->right()), 0);
        llvm::Value* null = llvm::ConstantPointerNull::get(pointerType);
        llvm::Value* one = llvm::ConstantInt::get(_context, llvm::APInt(64, (uint64_t) 1, true));
        llvm::Value* size = _builder.CreateGEP(null, one);
        _valueOfLastExpr = _builder.CreatePtrToInt(size, llvm::Type::getInt64Ty(_context));
    }

    void LLVMEmitVisitor::visitCastExpr(Cast* expr) {
        auto value = emit(expr->right());
        auto from = expr->right()->type();
        auto to = expr->targetType();

        _valueOfLastExpr = emitCast(value, from, to);
    }

    void LLVMEmitVisitor::visitVariableExpr(Variable* expr) {
        switch (expr->resolvesTo()->kind()) {
            case StatementKind::VAR_DECL:
            case StatementKind::PARAMETER:
            {
                llvm::Value* value = _namedValues[expr->resolvesTo()];
                _valueOfLastExpr = _builder.CreateLoad(value);
                break;
            }
            case StatementKind::FUNCTION:
            default:
            {
                _valueOfLastExpr = _namedValues[expr->resolvesTo()];
                break;
            }
        }
    }

    void LLVMEmitVisitor::visitNumberLiteral(NumberLiteral* expr) {
        switch (expr->typeKind()) {
            case PrimitiveTypeKind::I64:
                _valueOfLastExpr = llvm::ConstantInt::get(_context, llvm::APInt(64, (uint64_t) expr->i64(), true));
                break;
            case PrimitiveTypeKind::U64:
                _valueOfLastExpr = llvm::ConstantInt::get(_context, llvm::APInt(64, expr->u64(), false));
                break;
            case PrimitiveTypeKind::F64:
                _valueOfLastExpr = llvm::ConstantFP::get(_context, llvm::APFloat(expr->f64()));
                break;
            default:
                // TODO: Error handling
                assert(false);
                break;
        }
    }

    void LLVMEmitVisitor::visitBoolLiteral(BoolLiteral* expr) {
        _valueOfLastExpr = llvm::ConstantInt::get(_context, llvm::APInt(1, (uint64_t) expr->value()));
    }

    void LLVMEmitVisitor::visitStringLiteral(StringLiteral* expr) {
        _valueOfLastExpr = _builder.CreateGlobalStringPtr(expr->value(), "globalString");
    }

    void LLVMEmitVisitor::visitCharacterLiteral(CharacterLiteral* expr) {
        _valueOfLastExpr = llvm::ConstantInt::get(_context, llvm::APInt(8, (uint32_t) expr->value()));
    }
}