#include "llvm_emit_visitor.h"

#include "ast/module.h"
#include "ast/stmt.h"
#include "ast/expr.h"

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/InstrTypes.h"

#include "llvm/Support/TargetSelect.h"

namespace klong {

    bool LLVMEmitVisitor::_initialized = false;

    LLVMEmitVisitor::LLVMEmitVisitor() {
        if (!_initialized) {
            llvm::InitializeAllTargetInfos();
            llvm::InitializeAllTargets();
            llvm::InitializeAllTargetMCs();
            llvm::InitializeAllAsmParsers();
            llvm::InitializeAllAsmPrinters();
            _initialized = true;
        }
    }

    llvm::Module* LLVMEmitVisitor::getModule() {
        return _module.get();
    }

    llvm::Type* LLVMEmitVisitor::getLLVMType(Type *type) {
        type->accept(this);
        return _valueOfLastType;
    }

    llvm::Value* LLVMEmitVisitor::emit(Expr* expr) {
        expr->accept(this);
        return _valueOfLastExpr;
    }

    void LLVMEmitVisitor::emit(Stmt* stmt) {
        // eliminate dead code after return
        bool blockChanged = IRBuilder.GetInsertBlock() != _previousBlock;
        if (blockChanged) {
            _previousBlock = IRBuilder.GetInsertBlock();
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
        auto targetType = getLLVMType(to);
        auto myPrimSourceType = dynamic_cast<PrimitiveType*>(from);
        auto myPrimTargetType = dynamic_cast<PrimitiveType*>(to);

        auto myPointerSourceType = dynamic_cast<PointerType*>(from);
        auto myPointerTargetType = dynamic_cast<PointerType*>(to);

        if (myPrimSourceType && myPrimTargetType) {
            if ((myPrimSourceType->isInteger() || myPrimSourceType->isBoolean()) &&
                (myPrimTargetType->isInteger() || myPrimTargetType->isBoolean())) {
                return IRBuilder.CreateIntCast(value, targetType, myPrimTargetType->isSigned());
            }

            if (myPrimSourceType->isFloat() && myPrimTargetType->isFloat()) {
                return IRBuilder.CreateFPCast(value, targetType);
            }

            if ((myPrimSourceType->isInteger() || myPrimSourceType->isBoolean()) && (myPrimTargetType->isFloat())) {
                if (myPrimSourceType->isSigned()) {
                    return IRBuilder.CreateSIToFP(value, targetType);
                } else {
                    return IRBuilder.CreateUIToFP(value, targetType);
                }
            }

            if ((myPrimSourceType->isFloat()) || (myPrimTargetType->isInteger() || myPrimTargetType->isBoolean())) {
                if (myPrimTargetType->isSigned()) {
                    return IRBuilder.CreateFPToSI(value, targetType);
                } else {
                    return IRBuilder.CreateFPToUI(value, targetType);
                }
            }
        }

        if (myPrimSourceType && myPrimSourceType->isFloat() && myPointerTargetType) {
            auto valueAsU64 = IRBuilder.CreateFPToUI(value, llvm::IntegerType::getInt64Ty(context));
            return IRBuilder.CreateIntToPtr(valueAsU64, targetType);
        }

        if (myPointerSourceType && myPrimTargetType && myPrimTargetType->isFloat()) {
            auto valueAsU64 = IRBuilder.CreatePtrToInt(value, llvm::IntegerType::getInt64Ty(context));
            return IRBuilder.CreateUIToFP(valueAsU64, targetType);
        }

        if (myPointerSourceType && myPointerTargetType) {
            return IRBuilder.CreatePointerCast(value, targetType);
        }
        if (myPrimSourceType && myPrimSourceType->isInteger() && myPointerTargetType) {
            return IRBuilder.CreateIntToPtr(value, targetType);
        }
        if (myPointerSourceType && myPrimTargetType && myPrimTargetType->isInteger()) {
            return IRBuilder.CreatePtrToInt(value, targetType);
        }

        // Type conversion failed
        assert(false);
        return value;
    }

    void LLVMEmitVisitor::visitModule(Module* module) {
        _module = llvm::make_unique<llvm::Module>(module->filename(), context);

        for (auto& stmt : module->statements()) {
            if (stmt->kind() == StatementKind::FUNCTION) {
                auto function = dynamic_cast<Function*>(stmt);
                auto linkage = function->isPublic() ?
                        llvm::Function::ExternalLinkage : llvm::Function::InternalLinkage;

                auto functionType = (llvm::FunctionType*) getLLVMType(function->functionType());

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
        auto type = getLLVMType(stmt->type());

        bool isFunction = stmt->type()->kind() == TypeKind::FUNCTION;
        bool isPointer = stmt->type()->kind() == TypeKind::POINTER;
        if (isPointer) {
            auto pointerType = dynamic_cast<PointerType*>(stmt->type());
            auto pointsToType = pointerType->pointsTo();
            if (pointsToType->kind() == TypeKind::FUNCTION) {
                isFunction = true;
                type = getLLVMType(pointsToType);
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
        llvm::BasicBlock* bb = llvm::BasicBlock::Create(context, "entry", function);
        IRBuilder.SetInsertPoint(bb);

        {
            size_t i = 0;
            for (auto& arg : function->args()) {
                llvm::Type* paramType = getLLVMType(stmt->params()[i]->type());
                auto param = IRBuilder.CreateAlloca(paramType);
                IRBuilder.CreateStore(&arg, param);
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
        llvm::Value* condV = emit(stmt->condition());
        llvm::Function* function = IRBuilder.GetInsertBlock()->getParent();
        llvm::BasicBlock* thenBB = llvm::BasicBlock::Create(context, "thenBranch", function);
        llvm::BasicBlock* elseBB = llvm::BasicBlock::Create(context, "elseBranch", function);
        llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(context, "mergeBranch", function);

        IRBuilder.CreateCondBr(condV, thenBB, elseBB);

        IRBuilder.SetInsertPoint(thenBB);
        emit(stmt->thenBranch());
        IRBuilder.CreateBr(mergeBB);

        IRBuilder.SetInsertPoint(elseBB);
        if (stmt->elseBranch() != nullptr) {
            emit(stmt->elseBranch());
        }
        IRBuilder.CreateBr(mergeBB);

        IRBuilder.SetInsertPoint(mergeBB);
        if (stmt->isMergeUnreachable()) {
            IRBuilder.CreateUnreachable();
        }
    }

    void LLVMEmitVisitor::visitReturnStmt(Return* stmt) {
        _valueOfLastExpr = nullptr;
        if (stmt->value() != nullptr) {
            emit(stmt->value());
        }
        IRBuilder.CreateRet(_valueOfLastExpr);
        _eliminateDeadCodeInCurrentBlock = true;
    }

    void LLVMEmitVisitor::visitVarDeclStmt(VariableDeclaration* stmt) {
        llvm::Type* type = getLLVMType(stmt->type());

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
            auto stackPtr = IRBuilder.CreateAlloca(type);
            _namedValues[stmt] = stackPtr;
            auto value = emit(stmt->initializer());
            IRBuilder.CreateStore(value, stackPtr);
        }
    }

    void LLVMEmitVisitor::visitWhileStmt(While* stmt) {
        llvm::Function* function = IRBuilder.GetInsertBlock()->getParent();

        llvm::BasicBlock* whileCondBB = llvm::BasicBlock::Create(context, "whileCond", function);
        llvm::BasicBlock* whileBodyBB = llvm::BasicBlock::Create(context, "whileBody", function);
        llvm::BasicBlock* mergeWhileBB = llvm::BasicBlock::Create(context, "mergeWhile", function);

        IRBuilder.CreateBr(whileCondBB);

        IRBuilder.SetInsertPoint(whileCondBB);
        llvm::Value* condV = emit(stmt->condition());
        IRBuilder.CreateCondBr(condV, whileBodyBB, mergeWhileBB);

        IRBuilder.SetInsertPoint(whileBodyBB);

        emit(stmt->body());
        IRBuilder.CreateBr(whileCondBB);

        IRBuilder.SetInsertPoint(mergeWhileBB);
    }

    void LLVMEmitVisitor::visitForStmt(For* stmt) {
        llvm::Function* function = IRBuilder.GetInsertBlock()->getParent();

        llvm::BasicBlock* forInitBB = llvm::BasicBlock::Create(context, "forInit", function);
        llvm::BasicBlock* forCondBB = llvm::BasicBlock::Create(context, "forCond", function);
        llvm::BasicBlock* forBodyBB = llvm::BasicBlock::Create(context, "forBody", function);
        llvm::BasicBlock* mergeForBB = llvm::BasicBlock::Create(context, "mergeFor", function);

        IRBuilder.CreateBr(forInitBB);

        IRBuilder.SetInsertPoint(forInitBB);
        if (stmt->initializer() != nullptr) {
            emit(stmt->initializer());
        }
        IRBuilder.CreateBr(forCondBB);

        IRBuilder.SetInsertPoint(forCondBB);
        llvm::Value* condV = emit(stmt->condition());
        IRBuilder.CreateCondBr(condV, forBodyBB, mergeForBB);

        IRBuilder.SetInsertPoint(forBodyBB);
        emit(stmt->body());
        if (stmt->increment() != nullptr) {
            emit(stmt->increment());
        }
        IRBuilder.CreateBr(forCondBB);

        IRBuilder.SetInsertPoint(mergeForBB);
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
        IRBuilder.CreateStore(value, address);
        _valueOfLastExpr = value;
    }

    void LLVMEmitVisitor::visitBinaryExpr(Binary* expr) {
        llvm::Value* left = emit(expr->left());
        llvm::Value* right = emit(expr->right());
        auto leftType = dynamic_cast<PrimitiveType*>(expr->left()->type());
        auto targetType = dynamic_cast<PrimitiveType*>(expr->type());
        if (targetType->isInteger()) {
            if (!expr->type()->isEqual(expr->left()->type())) {
                left = emitCast(left, expr->left()->type(), expr->type());
            }
            if (!expr->type()->isEqual(expr->right()->type())) {
                right = emitCast(right, expr->right()->type(), expr->type());
            }
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
                    if (targetType->isSigned()) {
                        _valueOfLastExpr = IRBuilder.CreateSDiv(left, right);
                    } else {
                        _valueOfLastExpr = IRBuilder.CreateUDiv(left, right);
                    }
                    break;
                case BinaryOperation::MODULO:
                    if (targetType->isSigned()) {
                        _valueOfLastExpr = IRBuilder.CreateSRem(left, right);
                    } else {
                        _valueOfLastExpr = IRBuilder.CreateURem(left, right);
                    }
                    break;
                case BinaryOperation::LSL:
                    _valueOfLastExpr = IRBuilder.CreateShl(left, right);
                    break;
                case BinaryOperation::LSR:
                    _valueOfLastExpr = IRBuilder.CreateLShr(left, right);
                    break;
                case BinaryOperation::ASR:
                    _valueOfLastExpr = IRBuilder.CreateAShr(left, right);
                    break;
                case BinaryOperation::AND:
                    _valueOfLastExpr = IRBuilder.CreateAnd(left, right);
                    break;
                case BinaryOperation::XOR:
                    _valueOfLastExpr = IRBuilder.CreateXor(left, right);
                    break;
                case BinaryOperation::OR:
                    _valueOfLastExpr = IRBuilder.CreateOr(left, right);
                    break;
                default:
                    assert(false);
                    break;
            }
            return;
        }
        if (targetType->isFloat()) {
            if (!expr->type()->isEqual(expr->left()->type())) {
                left = emitCast(left, expr->left()->type(), expr->type());
            }
            if (!expr->type()->isEqual(expr->right()->type())) {
                right = emitCast(right, expr->right()->type(), expr->type());
            }
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
                case BinaryOperation::MODULO:
                    _valueOfLastExpr = IRBuilder.CreateFRem(left, right);
                    break;
                default:
                    assert(false);
                    break;
            }
            return;
        }
        if (leftType->isInteger() && targetType->isBoolean()) {
            switch (expr->op()) {
                case BinaryOperation::EQUALITY:
                    _valueOfLastExpr = IRBuilder.CreateICmpEQ(left, right);
                    break;
                case BinaryOperation::INEQUALITY:
                    _valueOfLastExpr = IRBuilder.CreateICmpNE(left, right);
                    break;
                case BinaryOperation::LESS_THAN:
                    if (leftType->isSigned()) {
                        _valueOfLastExpr = IRBuilder.CreateICmpSLT(left, right);
                    } else {
                        _valueOfLastExpr = IRBuilder.CreateICmpULT(left, right);
                    }
                    break;
                case BinaryOperation::LESS_EQUAL:
                    if (leftType->isSigned()) {
                        _valueOfLastExpr = IRBuilder.CreateICmpSLE(left, right);
                    } else {
                        _valueOfLastExpr = IRBuilder.CreateICmpULE(left, right);
                    }
                    break;
                case BinaryOperation::GREATER_THAN:
                    if (leftType->isSigned()) {
                        _valueOfLastExpr = IRBuilder.CreateICmpSGT(left, right);
                    } else {
                        _valueOfLastExpr = IRBuilder.CreateICmpUGT(left, right);
                    }
                    break;
                case BinaryOperation::GREATER_EQUAL:
                    if (leftType->isSigned()) {
                        _valueOfLastExpr = IRBuilder.CreateICmpSGE(left, right);
                    } else {
                        _valueOfLastExpr = IRBuilder.CreateICmpUGE(left, right);
                    }
                    break;
                default:
                    assert(false);
                    break;
            }
        }
        if (leftType->isFloat() && targetType->isBoolean()) {
            switch (expr->op()) {
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
        _valueOfLastExpr = IRBuilder.CreateCall(calleeF, argsV);
    }

    void LLVMEmitVisitor::visitGroupingExpr(Grouping* expr) {
        _valueOfLastExpr = emit(expr->expression());
    }

    void LLVMEmitVisitor::visitLogicalExpr(Logical* expr) {
        llvm::Value* left = emit(expr->left());
        llvm::Value* right = emit(expr->right());
        switch (expr->op()) {
            case LogicalOperation::AND:
                _valueOfLastExpr = IRBuilder.CreateAnd(left, right);
                break;
            case LogicalOperation::OR:
                _valueOfLastExpr = IRBuilder.CreateOr(left, right);
                break;
        }
    }

    void LLVMEmitVisitor::visitUnaryExpr(Unary* expr) {
        llvm::Value* right = emit(expr->right());
        switch(expr->op()) {
            case UnaryOperation::MINUS:
                _valueOfLastExpr = IRBuilder.CreateNeg(right);
                break;
            case UnaryOperation::NOT:
                _valueOfLastExpr = IRBuilder.CreateNot(right);
                break;
            case UnaryOperation::DEREF:
                _valueOfLastExpr = IRBuilder.CreateLoad(right);
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
        auto pointerType = llvm::PointerType::get(getLLVMType(expr->right()), 0);
        llvm::Value* null = llvm::ConstantPointerNull::get(pointerType);
        llvm::Value* one = llvm::ConstantInt::get(context, llvm::APInt(64, (uint64_t) 1, true));
        llvm::Value* size = IRBuilder.CreateGEP(null, one);
        _valueOfLastExpr = IRBuilder.CreatePtrToInt(size, llvm::Type::getInt64Ty(context));
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
                _valueOfLastExpr = IRBuilder.CreateLoad(value);
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
                assert(false);
                break;
        }
    }

    void LLVMEmitVisitor::visitBoolLiteral(BoolLiteral* expr) {
        switch (expr->literalType()) {
            case PrimitiveTypeKind::BOOL:
                _valueOfLastExpr = llvm::ConstantInt::get(context, llvm::APInt(1, (uint64_t) expr->value()));
                break;
            default:
                // TODO: Error handling
                assert(false);
                break;
        }
    }

    void LLVMEmitVisitor::visitStringLiteral(StringLiteral* expr) {
        switch (expr->literalType()) {
            case PrimitiveTypeKind::STRING:
                _valueOfLastExpr = llvm::ConstantDataArray::getString(context, expr->value(), true);
                break;
            default:
                // TODO: Error handling
                assert(false);
                break;
        }
    }

    void LLVMEmitVisitor::visitCharacterLiteral(CharacterLiteral* expr) {
        switch (expr->literalType()) {
            case PrimitiveTypeKind::I8:
                _valueOfLastExpr = llvm::ConstantInt::get(context, llvm::APInt(8, (uint32_t) expr->value()));
                break;
            default:
                // TODO: Error handling
                assert(false);
                break;
        }
    }

    void LLVMEmitVisitor::visitFunctionType(FunctionType* type) {

        std::vector<llvm::Type*> paramTypes;

        auto prevOuterType = _outerType;
        _outerType = TypeKind::FUNCTION;

        for (auto& paramType : type->paramTypes()) {
            paramTypes.push_back(getLLVMType(paramType));
        }
        llvm::Type* returnType = getLLVMType(type->returnType());

        _outerType = prevOuterType;

        _valueOfLastType = llvm::FunctionType::get(returnType, paramTypes, false);
    }

    void LLVMEmitVisitor::visitPrimitiveType(PrimitiveType *type) {
        switch (type->type()) {
            case PrimitiveTypeKind::VOID:
                {
                    if (_outerType == TypeKind::POINTER) {
                        _valueOfLastType = llvm::Type::getInt8Ty(context);
                        break;
                    }
                    _valueOfLastType = llvm::Type::getVoidTy(context);
                    break;
                }
            case PrimitiveTypeKind::BOOL:
                _valueOfLastType = llvm::Type::getInt1Ty(context);
                break;
            case PrimitiveTypeKind::I8:
            case PrimitiveTypeKind::U8:
                _valueOfLastType = llvm::Type::getInt8Ty(context);
                break;
            case PrimitiveTypeKind::I16:
            case PrimitiveTypeKind::U16:
                _valueOfLastType = llvm::Type::getInt16Ty(context);
                break;
            case PrimitiveTypeKind::I32:
            case PrimitiveTypeKind::U32:
                _valueOfLastType = llvm::Type::getInt32Ty(context);
                break;
            case PrimitiveTypeKind::I64:
            case PrimitiveTypeKind::U64:
                _valueOfLastType = llvm::Type::getInt64Ty(context);
                break;
            case PrimitiveTypeKind::F32:
                _valueOfLastType = llvm::Type::getFloatTy(context);
                break;
            case PrimitiveTypeKind::F64:
                _valueOfLastType = llvm::Type::getDoubleTy(context);
                break;
            default:
                // TODO: how to handle the other types
                assert(false);
        }
    }

    void LLVMEmitVisitor::visitPointerType(klong::PointerType *type) {
        auto prevOuterType = _outerType;
        _outerType = TypeKind::POINTER;
        auto innerType = getLLVMType(type->pointsTo());
        _outerType = prevOuterType;
        _valueOfLastType = llvm::PointerType::get(innerType, 0);
    }

    void LLVMEmitVisitor::visitSimpleType(SimpleType *type) {
        // TODO: how to handle the other types
        auto prevOuterType = _outerType;
        _outerType = TypeKind::SIMPLE;
        (void) type;
        _outerType = prevOuterType;
        assert(false);
    }
}