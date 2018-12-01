#include "llvm_emit_visitor.h"

#include <iterator>

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

    LLVMEmitVisitor::LLVMEmitVisitor(const llvm::DataLayout dataLayout) :
        _context(),
        _builder(_context),
        _typeEmitVisitor(_context, dataLayout) {

    }

    llvm::Module* LLVMEmitVisitor::getModule() {
        return _module.get();
    }

    llvm::Value* LLVMEmitVisitor::emitCodeL(Expr* expr) {
        _isCodeL = true;
        _valueOfLastExpr = nullptr;
        expr->accept(this);
        return _valueOfLastExpr;
    }

    llvm::Value* LLVMEmitVisitor::emitCodeR(Expr *expr) {
        _isCodeL = false;
        _valueOfLastExpr = nullptr;
        expr->accept(this);
        return _valueOfLastExpr;
    }

    void LLVMEmitVisitor::emitCode(Stmt *stmt) {
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
        _deferScopes.emplace_back(std::vector<Stmt*>());
        for (auto& stmt : statements) {
            emitCode(stmt);
        }
        emitLocalDefers();
        assert(!_deferScopes.empty());
        _deferScopes.pop_back();
    }

    void LLVMEmitVisitor::emitLocalDefers() {
        auto& back = _deferScopes.back();
        for (uint64_t i = back.size(); i-- > 0; ) {
            emitCode(back[i]);
        }
    }

    void LLVMEmitVisitor::emitAllDefers() {
        for (uint64_t x = _deferScopes.size(); x-- > 0; ) {
            auto& deferScope = _deferScopes[x];
            for (uint64_t i = deferScope.size(); i-- > 0; ) {
                emitCode(deferScope[i]);
            }
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

    std::shared_ptr<ExternalDeclaration> LLVMEmitVisitor::translateToExternalDeclaration(Stmt* stmt) {
        auto function = dynamic_cast<Function*>(stmt);
        auto varDecl = dynamic_cast<VariableDeclaration*>(stmt);
        auto extDecl = dynamic_cast<ExternalDeclaration*>(stmt);
        if (function) {
            return std::make_shared<ExternalDeclaration>(function->sourceRange(),
                    function->name(), std::shared_ptr<Type>(function->functionType()->clone()));
        }
        if (varDecl) {
            return std::make_shared<ExternalDeclaration>(varDecl->sourceRange(),
                    varDecl->name(), std::shared_ptr<Type>(varDecl->type()->clone()));
        }
        if (extDecl) {
            return std::make_shared<ExternalDeclaration>(*extDecl);
        }
        assert(false);
        return nullptr;
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
        emitCodeR(stmt->expression());
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

    void LLVMEmitVisitor::visitImportStmt(Import *stmt) {
        // nothing to do here
        (void) stmt;
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

        // add a return statement if functionType is void
        // and the last block has no terminator
        if (Type::isVoid(stmt->functionType()->returnType())
            && !_builder.GetInsertBlock()->getTerminator()) {
            _builder.CreateRet(nullptr);
        }
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

		auto condV = emitCodeR(stmt->condition());
        _builder.CreateCondBr(condV, thenBB, elseBB);

        _builder.SetInsertPoint(thenBB);
        emitCode(stmt->thenBranch());
		if (!_eliminateDeadCodeInCurrentBlock) {
			_builder.CreateBr(mergeBB);
		}

		function->getBasicBlockList().push_back(elseBB);
        _builder.SetInsertPoint(elseBB);
        if (stmt->elseBranch() != nullptr) {
            emitCode(stmt->elseBranch());
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
        emitAllDefers();
        _valueOfLastExpr = nullptr;
        if (stmt->value() != nullptr) {
            emitCodeR(stmt->value());
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
            if (stmt->initializer()) {
                global->setInitializer((llvm::Constant*) emitCodeR(stmt->initializer()));
            }
            _namedValues[stmt] = global;
        } else {
            auto stackPtr = _builder.CreateAlloca(type);
            _namedValues[stmt] = stackPtr;
            if (stmt->initializer()) {
                auto value = emitCodeR(stmt->initializer());
                _builder.CreateStore(value, stackPtr);
            }
        }
    }

	void LLVMEmitVisitor::visitStructDeclStmt(StructDeclaration* stmt) {
		// nothing to do here
        (void) stmt;
	}

    void LLVMEmitVisitor::visitUnionDeclStmt(UnionDeclaration* stmt) {
        // nothing to do here
        (void) stmt;
    }

	void LLVMEmitVisitor::visitEnumDeclStmt(EnumDeclaration* stmt) {
		// nothing to do here
		(void) stmt;
	}

	void LLVMEmitVisitor::visitCustomMemberStmt(CustomMember* stmt) {
        // nothing to do here
        (void) stmt;
	}

    void LLVMEmitVisitor::visitWhileStmt(While* stmt) {
        llvm::Function* function = _builder.GetInsertBlock()->getParent();

        auto whileCondBB = llvm::BasicBlock::Create(_context, "whileCond", function);
        auto whileBodyBB = llvm::BasicBlock::Create(_context, "whileBody");
        auto mergeWhileBB = llvm::BasicBlock::Create(_context, "mergeWhile");

        _builder.CreateBr(whileCondBB);

        _builder.SetInsertPoint(whileCondBB);
        llvm::Value* condV = emitCodeR(stmt->condition());
        _builder.CreateCondBr(condV, whileBodyBB, mergeWhileBB);

        function->getBasicBlockList().push_back(whileBodyBB);
        _builder.SetInsertPoint(whileBodyBB);

        auto prevBreakJmpTarget = _breakJmpTarget;
        auto prevContinueJmpTarget = _continueJmpTarget;
        _breakJmpTarget = mergeWhileBB;
        _continueJmpTarget = whileCondBB;

        emitCode(stmt->body());

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
            emitCode(stmt->initializer());
        }
        _builder.CreateBr(forCondBB);

        _builder.SetInsertPoint(forCondBB);
        llvm::Value* condV = emitCodeR(stmt->condition());
        _builder.CreateCondBr(condV, forBodyBB, mergeForBB);

        function->getBasicBlockList().push_back(forBodyBB);
        _builder.SetInsertPoint(forBodyBB);
        auto prevBreakJmpTarget = _breakJmpTarget;
        auto prevContinueJmpTarget = _continueJmpTarget;
        _breakJmpTarget = mergeForBB;
        _continueJmpTarget = forIncBB;

        emitCode(stmt->body());

        _continueJmpTarget = prevContinueJmpTarget;
        _breakJmpTarget = prevBreakJmpTarget;

		// we need to check here, if the body contains any terminators
		if (!_eliminateDeadCodeInCurrentBlock) {
            _builder.CreateBr(forIncBB);
        }

        function->getBasicBlockList().push_back(forIncBB);
        _builder.SetInsertPoint(forIncBB);
        if (stmt->increment() != nullptr) {
            emitCodeR(stmt->increment());
        }
        _builder.CreateBr(forCondBB);

        function->getBasicBlockList().push_back(mergeForBB);
        _builder.SetInsertPoint(mergeForBB);
    }

    void LLVMEmitVisitor::visitBreakStmt(Break* stmt) {
        (void) stmt;
        emitLocalDefers();
        _builder.CreateBr(_breakJmpTarget);
		_eliminateDeadCodeInCurrentBlock = true;
    }

    void LLVMEmitVisitor::visitContinueStmt(Continue* stmt) {
        (void) stmt;
        emitLocalDefers();
        _builder.CreateBr(_continueJmpTarget);
		_eliminateDeadCodeInCurrentBlock = true;
    }

    void LLVMEmitVisitor::visitDeferStmt(Defer* stmt) {
        _deferScopes.back().push_back(stmt->stmtToDefer());
    }

    void LLVMEmitVisitor::visitCommentStmt(Comment* stmt) {
        (void) stmt;
    }

    void LLVMEmitVisitor::visitAssignExpr(Assign* expr) {
        llvm::Value* address = nullptr;
        if (expr->isTargetVariable()) {
            address = emitCodeL(expr->target());
        } else {
            address = emitCodeL(expr->targetExpr());
        }
        auto value = emitCodeR(expr->value());
        _builder.CreateStore(value, address);
        _valueOfLastExpr = value;
    }

    void LLVMEmitVisitor::visitBinaryExpr(Binary* expr) {
        auto left = emitCodeR(expr->left());
        auto right = emitCodeR(expr->right());
        auto leftType = dynamic_cast<PrimitiveType*>(expr->left()->type());
        auto targetType = dynamic_cast<PrimitiveType*>(expr->type());
		if (expr->castToType()) {
			left = emitCast(left, expr->left()->type(), expr->castToType());
			right = emitCast(right, expr->right()->type(), expr->castToType());
		}

        if (targetType && targetType->isInteger()) {
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
        if (leftType && targetType && Type::isInteger(expr->castToType()) && targetType->isBoolean()) {
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
        if (leftType && targetType && Type::isFloat(expr->castToType()) && targetType->isBoolean()) {
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

		// ONLY ENUMS
		// (this is already checked by the typechecker)
		auto leftCustomType = dynamic_cast<CustomType*>(expr->left()->type());
		auto rightCustomType = dynamic_cast<CustomType*>(expr->right()->type());
		if (leftCustomType && rightCustomType) {
			switch (expr->op()) {
			case BinaryOperation::EQUALITY:
				_valueOfLastExpr = _builder.CreateICmpEQ(left, right);
				break;
			case BinaryOperation::INEQUALITY:
				_valueOfLastExpr = _builder.CreateICmpNE(left, right);
				break;
			default:
				assert(false);
			}
		}
    }

    void LLVMEmitVisitor::visitCallExpr(Call* expr) {
        auto calleeF = emitCodeR(expr->callee());
        std::vector<llvm::Value*> argsV;
        for (auto& arg : expr->args()) {
            argsV.push_back(emitCodeR(arg));
        }
        _valueOfLastExpr = _builder.CreateCall(calleeF, argsV);
    }

    void LLVMEmitVisitor::visitGroupingExpr(Grouping* expr) {
        _valueOfLastExpr = emitCodeR(expr->expression());
    }

	void LLVMEmitVisitor::visitSubscriptExpr(Subscript* expr) {
        auto isCodeL = _isCodeL;
		// implement subscript operator
		auto target = emitCodeR(expr->target());
		auto index = emitCodeR(expr->index());
		auto pointerToElement = _builder.CreateGEP(target, index);
		if (isCodeL) {
			_valueOfLastExpr = pointerToElement;
		} else {
			_valueOfLastExpr = _builder.CreateLoad(pointerToElement);
		}
	}

	void LLVMEmitVisitor::visitMemberAccessExpr(MemberAccess* expr) {
        auto isCodeL = _isCodeL;

        llvm::Value* address = nullptr;
        auto targetVal = emitCodeL(expr->target());

        // this is a workaround
        // because we cannot directly access the value of a call expr
        // see: https://github.com/jdillenkofer/klong/issues/40
        {
            Expr* realMemberExpr = expr->target();
            while(realMemberExpr->kind() == ExprKind::CAST || realMemberExpr->kind() == ExprKind::GROUPING) {
                auto castExpr = dynamic_cast<Cast*>(realMemberExpr);
                if (castExpr) {
                    realMemberExpr = castExpr->right();
                }
                auto groupingExpr = dynamic_cast<Grouping*>(realMemberExpr);
                if (groupingExpr) {
                    realMemberExpr = groupingExpr->expression();
                }
            }

            auto type = _typeEmitVisitor.getLLVMType(realMemberExpr->type());

            if (realMemberExpr->kind() == ExprKind::CALL) {
                auto localAlloc = _builder.CreateAlloca(type);
                _builder.CreateStore(targetVal, localAlloc);
                targetVal = localAlloc;
            }
        }

        {
            auto customType = dynamic_cast<CustomType*>(expr->target()->type());
            auto pointerType = dynamic_cast<PointerType*>(expr->target()->type());
            if (pointerType) {
                customType = dynamic_cast<CustomType*>(pointerType->pointsTo());
                targetVal = _builder.CreateLoad(targetVal);
            }

            auto declarationType = customType->resolvesTo();
            switch (declarationType->typeDeclarationKind()) {
                case TypeDeclarationKind::STRUCT: {
                    auto structDecl = dynamic_cast<StructDeclaration*>(declarationType);
                    auto memberIndex = structDecl->findMemberIndex(expr->value()).value();
                    address = _builder.CreateStructGEP(targetVal, memberIndex);
                    address = _builder.CreateBitCast(address,
                            llvm::PointerType::get(_typeEmitVisitor.getLLVMType(expr->type()), 0));
                    break;
                }
                case TypeDeclarationKind::UNION: {
                    address = targetVal;
                    address = _builder.CreateBitCast(address,
                            llvm::PointerType::get(_typeEmitVisitor.getLLVMType(expr->type()), 0));
                    break;
                }
                default:
                    assert(false);
            }
        }

        if (isCodeL) {
            _valueOfLastExpr = address;
        } else {
            _valueOfLastExpr = _builder.CreateLoad(address);
        }
    }

	void LLVMEmitVisitor::visitEnumAccessExpr(EnumAccess* expr) {
		auto& value = expr->value();
		auto enumType = dynamic_cast<EnumDeclaration*>(expr->target()->resolvesTo());
		auto enumValues = enumType->values();
		auto it = std::find(enumValues.begin(), enumValues.end(), value);
		if (it == enumValues.end())
		{
			// no such value in enum
			assert(false);
		}
		else
		{
			auto index = std::distance(enumValues.begin(), it);
			_valueOfLastExpr = llvm::ConstantInt::get(_context, llvm::APInt(32, (uint32_t)index, true));
		}
	}

    void LLVMEmitVisitor::visitLogicalExpr(Logical* expr) {
        auto left = emitCodeR(expr->left());
        auto right = emitCodeR(expr->right());
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
        auto isCodeL = _isCodeL;

        if (isCodeL) {
            switch(expr->op()) {
                case UnaryOperation::DEREF:
                        // ignore the deref operator and
                        // just get the address of the variable
                        _valueOfLastExpr = emitCodeR(expr->right());
                        break;
                default:
                    assert(false);
                    break;
            }
        } else {
			switch(expr->op()) {
                case UnaryOperation::MINUS:
                    _valueOfLastExpr = _builder.CreateNeg(emitCodeR(expr->right()));
                    break;
                case UnaryOperation::NOT:
                    _valueOfLastExpr = _builder.CreateNot(emitCodeR(expr->right()));
                    break;
                case UnaryOperation::DEREF:
                    _valueOfLastExpr = _builder.CreateLoad(emitCodeR(expr->right()));
                    break;
                case UnaryOperation::ADDRESS_OF:
                    _valueOfLastExpr = emitCodeL(expr->right());
                    break;
            }
        }

    }

    llvm::Value* LLVMEmitVisitor::getVariableAddress(Expr* expr) {
        auto variable = dynamic_cast<Variable*>(expr);
        assert(variable != nullptr);
        auto address = _namedValues[variable->resolvesTo()];
        if (Type::isPointer(expr->type())) {
            auto asPointer = dynamic_cast<PointerType*>(expr->type());
            if (asPointer->isArray()) {
                auto clonedPtr = std::shared_ptr<Type>(asPointer->clone());
                address = _builder.CreateBitCast(address, _typeEmitVisitor.getLLVMType(clonedPtr.get()));
            }
        }
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
        auto value = emitCodeR(expr->right());
        auto from = expr->right()->type();
        auto to = expr->targetType();

        _valueOfLastExpr = emitCast(value, from, to);
    }

    void LLVMEmitVisitor::visitVariableExpr(Variable* expr) {
        if (_isCodeL) {
            _valueOfLastExpr = getVariableAddress(expr);
        } else {
            switch (expr->resolvesTo()->kind()) {
                case StatementKind::VAR_DECL:
                case StatementKind::PARAMETER:
                {
                    llvm::Value* value = _namedValues[expr->resolvesTo()];

                    if (!value) {
                        // this doesn't resolve to a value, if it is from another module
                        auto asExternalDecl = translateToExternalDeclaration(expr->resolvesTo());
                        emitCode(asExternalDecl.get());
                        value = _namedValues[asExternalDecl.get()];
                    }
                    if (expr->resolvesTo()->kind() == StatementKind::VAR_DECL) {
                        auto varDecl = dynamic_cast<VariableDeclaration*>(expr->resolvesTo());
                        if (Type::isPointer(varDecl->type())) {
                            auto pointerType = dynamic_cast<PointerType*>(varDecl->type());
                            if (pointerType->isArray()) {
                                llvm::Value* zero = llvm::ConstantInt::get(_context, llvm::APInt(64, (uint64_t) 0, true));
                                _valueOfLastExpr = _builder.CreateGEP(value, zero);
                                auto llvmPointerType = llvm::PointerType::get(
                                        _typeEmitVisitor.getLLVMType(pointerType->pointsTo()), 0);
                                _valueOfLastExpr = _builder.CreateBitCast(_valueOfLastExpr, llvmPointerType);
                                break;
                            }
                        }
                    }
                    _valueOfLastExpr = _builder.CreateLoad(value);
                    break;
                }
                case StatementKind::FUNCTION:
                default:
                {
                    _valueOfLastExpr = _namedValues[expr->resolvesTo()];
                    if (!_valueOfLastExpr) {
                        // this doesn't resolve to a value, if it is from another module
                        auto asExternalDecl = translateToExternalDeclaration(expr->resolvesTo());
                        emitCode(asExternalDecl.get());
                        _valueOfLastExpr = _namedValues[asExternalDecl.get()];
                    }
                    break;
                }
            }
        }
    }

    void LLVMEmitVisitor::visitNumberLiteral(NumberLiteral* expr) {
        switch (expr->typeKind()) {
            case PrimitiveTypeKind::I32:
                _valueOfLastExpr = llvm::ConstantInt::get(_context, llvm::APInt(32, (uint64_t) expr->i32(), true));
                break;
            case PrimitiveTypeKind::I64:
                _valueOfLastExpr = llvm::ConstantInt::get(_context, llvm::APInt(64, (uint64_t) expr->i64(), true));
                break;
            case PrimitiveTypeKind::U32:
                _valueOfLastExpr = llvm::ConstantInt::get(_context, llvm::APInt(32, (uint64_t) expr->u32(), false));
                break;
            case PrimitiveTypeKind::U64:
                _valueOfLastExpr = llvm::ConstantInt::get(_context, llvm::APInt(64, expr->u64(), false));
                break;
            case PrimitiveTypeKind::F32:
                _valueOfLastExpr = llvm::ConstantFP::get(_context, llvm::APFloat(expr->f32()));
                break;
            case PrimitiveTypeKind::F64:
                _valueOfLastExpr = llvm::ConstantFP::get(_context, llvm::APFloat(expr->f64()));
                break;
            default:
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

    void LLVMEmitVisitor::visitArrayLiteral(ArrayLiteral* expr) {
        std::vector<llvm::Constant*> values;
        for (auto& arrayVal : expr->values()) {
            values.push_back((llvm::Constant*) emitCodeR(arrayVal));
        }
		auto llvmArrayType = _typeEmitVisitor.getLLVMType(expr->type());
        _valueOfLastExpr = llvm::ConstantArray::get((llvm::ArrayType*)llvmArrayType, values);
    }

    void LLVMEmitVisitor::setSession(CompilationSession* session) {
        _session = session;
    }
}