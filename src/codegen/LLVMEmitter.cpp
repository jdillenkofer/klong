#include "LLVMEmitter.h"

#include "../parser/module.h"
#include "../parser/stmt.h"
#include "../parser/expr.h"

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"

#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"

namespace klong {

    bool LLVMEmitter::_initialized = false;

    LLVMEmitter::LLVMEmitter() {
        if (!_initialized) {
            llvm::InitializeAllTargetInfos();
            llvm::InitializeAllTargets();
            llvm::InitializeAllTargetMCs();
            llvm::InitializeAllAsmParsers();
            llvm::InitializeAllAsmPrinters();
            _initialized = true;
        }
    }

    llvm::Value* LLVMEmitter::emit(Expr* expr) {
        expr->accept(this);
        return _valueOfLastExpr;
    }

    llvm::Value* LLVMEmitter::emit(Stmt* stmt) {
        stmt->accept(this);
        return _valueOfLastExpr;
    }

    void LLVMEmitter::emitBlock(const std::vector<StmtPtr>& statements) {
        for (auto& stmt : statements) {
            stmt->accept(this);
        }
    }

    void LLVMEmitter::visitModule(Module* module) {
        _module = llvm::make_unique<llvm::Module>(module->filename(), context);

        for (auto& stmt : module->statements()) {
            if (stmt->kind() == StatementKind::FUNCTION) {
                std::shared_ptr<Function> function = std::dynamic_pointer_cast<Function>(stmt);
                function->functionType()->accept(this);
                auto functionType = (llvm::FunctionType*) _valueOfLastType;
                auto llvmFunction = llvm::Function::Create(functionType,
                        llvm::Function::ExternalLinkage, function->name(), _module.get());
                _namedValues[stmt.get()] = llvmFunction;
            }
        }

        for (auto& stmt : module->statements()) {
            stmt->accept(this);
        }
    }

    void LLVMEmitter::visitBlockStmt(Block* stmt) {
        emitBlock(stmt->statements());
    }

    void LLVMEmitter::visitExpressionStmt(Expression* stmt) {
        _valueOfLastExpr = emit(stmt->expression().get());
    }

    void LLVMEmitter::visitFunctionStmt(Function* stmt) {
        llvm::Function* function = _module->getFunction(stmt->name());

        // Function body
        llvm::BasicBlock* bb = llvm::BasicBlock::Create(context, "entry", function);
        IRBuilder.SetInsertPoint(bb);

        {
            size_t i = 0;
            for (auto& arg : function->args()) {
                stmt->params()[i]->type()->accept(this);
                llvm::Type* paramType = _valueOfLastType;
                auto param = IRBuilder.CreateAlloca(paramType);
                IRBuilder.CreateStore(&arg, param);
                _namedValues[stmt->params()[i].get()] = param;
                i++;
            }
        }

        for (auto& statement : stmt->body()) {
            emit(statement.get());
        }
        llvm::verifyFunction(*function);
    }

    void LLVMEmitter::visitParameterStmt(Parameter* stmt) {
        (void) stmt;
    }

    void LLVMEmitter::visitIfStmt(If* stmt) {
        llvm::Value* condV = emit(stmt->condition().get());
        llvm::Function* function = IRBuilder.GetInsertBlock()->getParent();
        llvm::BasicBlock* thenBB = llvm::BasicBlock::Create(context, "thenBranch", function);
        llvm::BasicBlock* elseBB = llvm::BasicBlock::Create(context, "elseBranch", function);
        llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(context, "mergeBranch", function);

        IRBuilder.CreateCondBr(condV, thenBB, elseBB);

        IRBuilder.SetInsertPoint(thenBB);
        emit(stmt->thenBranch().get());
        IRBuilder.CreateBr(mergeBB);

        IRBuilder.SetInsertPoint(elseBB);
        if (stmt->elseBranch() != nullptr) {
            emit(stmt->elseBranch().get());
        }
        IRBuilder.CreateBr(mergeBB);

        IRBuilder.SetInsertPoint(mergeBB);
    }

    void LLVMEmitter::visitPrintStmt(Print* stmt) {
        // TODO: strings and print
        (void) stmt;
    }

    void LLVMEmitter::visitReturnStmt(Return* stmt) {
        _valueOfLastExpr = nullptr;
        if (stmt->value() != nullptr) {
            emit(stmt->value().get());
        }
        IRBuilder.CreateRet(_valueOfLastExpr);
    }

    void LLVMEmitter::visitVarDeclStmt(VariableDeclaration* stmt) {
        stmt->type()->accept(this);
        llvm::Type* type = _valueOfLastType;

        if (stmt->isGlobal()) {
            _module->getOrInsertGlobal(stmt->name(), type);
            auto global = _module->getNamedGlobal(stmt->name());
            global->setLinkage(llvm::GlobalValue::ExternalLinkage);
            if (stmt->isConst()) {
                global->setConstant(true);
            }
            global->setInitializer((llvm::Constant*) emit(stmt->initializer().get()));
            _namedValues[stmt] = global;
        } else {
            auto stackPtr = IRBuilder.CreateAlloca(type);
            _namedValues[stmt] = stackPtr;
            auto value = emit(stmt->initializer().get());
            IRBuilder.CreateStore(value, stackPtr);
        }
    }

    void LLVMEmitter::visitWhileStmt(While* stmt) {
        llvm::Function* function = IRBuilder.GetInsertBlock()->getParent();

        llvm::BasicBlock* whileCondBB = llvm::BasicBlock::Create(context, "whileCond", function);
        llvm::BasicBlock* whileBodyBB = llvm::BasicBlock::Create(context, "whileBody", function);
        llvm::BasicBlock* mergeWhileBB = llvm::BasicBlock::Create(context, "mergeWhile", function);

        IRBuilder.CreateBr(whileCondBB);

        IRBuilder.SetInsertPoint(whileCondBB);
        llvm::Value* condV = emit(stmt->condition().get());
        IRBuilder.CreateCondBr(condV, whileBodyBB, mergeWhileBB);

        IRBuilder.SetInsertPoint(whileBodyBB);

        emit(stmt->body().get());
        IRBuilder.CreateBr(whileCondBB);

        IRBuilder.SetInsertPoint(mergeWhileBB);
    }

    void LLVMEmitter::visitForStmt(For* stmt) {
        llvm::Function* function = IRBuilder.GetInsertBlock()->getParent();

        llvm::BasicBlock* forInitBB = llvm::BasicBlock::Create(context, "forInit", function);
        llvm::BasicBlock* forCondBB = llvm::BasicBlock::Create(context, "forCond", function);
        llvm::BasicBlock* forBodyBB = llvm::BasicBlock::Create(context, "forBody", function);
        llvm::BasicBlock* mergeForBB = llvm::BasicBlock::Create(context, "mergeFor", function);

        IRBuilder.CreateBr(forInitBB);

        IRBuilder.SetInsertPoint(forInitBB);
        if (stmt->initializer() != nullptr) {
            emit(stmt->initializer().get());
        }
        IRBuilder.CreateBr(forCondBB);

        IRBuilder.SetInsertPoint(forCondBB);
        llvm::Value* condV = emit(stmt->condition().get());
        IRBuilder.CreateCondBr(condV, forBodyBB, mergeForBB);

        IRBuilder.SetInsertPoint(forBodyBB);
        emit(stmt->body().get());
        if (stmt->increment() != nullptr) {
            emit(stmt->increment().get());
        }
        IRBuilder.CreateBr(forCondBB);

        IRBuilder.SetInsertPoint(mergeForBB);
    }

    void LLVMEmitter::visitCommentStmt(Comment* stmt) {
        (void) stmt;
    }

    void LLVMEmitter::visitAssignExpr(Assign* expr) {
        _valueOfLastExpr = emit(expr->value().get());
        IRBuilder.CreateStore(_valueOfLastExpr, _namedValues[expr->target()->resolvesTo()]);
    }

    void LLVMEmitter::visitBinaryExpr(Binary* expr) {
        llvm::Value* left = emit(expr->left().get());
        llvm::Value* right = emit(expr->right().get());
        PrimitiveType* type = dynamic_cast<PrimitiveType*>(expr->left()->type().get());
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
                case BinaryOperation::MODULO:
                    if (type->isSigned()) {
                        _valueOfLastExpr = IRBuilder.CreateSRem(left, right);
                    } else {
                        _valueOfLastExpr = IRBuilder.CreateURem(left, right);
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
                case BinaryOperation::MODULO:
                    _valueOfLastExpr = IRBuilder.CreateFRem(left, right);
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
        auto calleeF = emit(expr->callee().get());
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
        std::vector<llvm::Type*> paramTypes;
        for (auto& t : type->paramTypes()) {
            t->accept(this);
            paramTypes.push_back(_valueOfLastType);
        }
        type->returnType()->accept(this);
        llvm::Type* returnType = _valueOfLastType;
        _valueOfLastType = llvm::FunctionType::get(returnType, paramTypes, false);
    }

    void LLVMEmitter::visitPrimitiveType(PrimitiveType *type) {
        switch (type->type()) {
            case PrimitiveTypeKind::VOID:
                _valueOfLastType = llvm::Type::getVoidTy(context);
                break;
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
                throw 5;
        }
    }

    void LLVMEmitter::visitPointerType(klong::PointerType *type) {
        type->pointsTo()->accept(this);
        _valueOfLastType = llvm::PointerType::get(_valueOfLastType, 0);
    }

    void LLVMEmitter::visitSimpleType(SimpleType *type) {
        // TODO: how to handle the other types
        (void) type;
    }

    void LLVMEmitter::printIR() {
        _module->print(llvm::outs(), nullptr);
    }

    bool LLVMEmitter::generateObjectFile(
            std::string outputFilename,
            std::string targetTriple,
            std::string cpu,
            std::string features) {
        std::string error;
        auto target = llvm::TargetRegistry::lookupTarget(targetTriple, error);

        llvm::TargetOptions opt;
        auto rm = llvm::Optional<llvm::Reloc::Model>();
        auto targetMachine = target->createTargetMachine(targetTriple, cpu, features, opt, rm);

        _module->setTargetTriple(targetTriple);
        _module->setDataLayout(targetMachine->createDataLayout());

        std::error_code error_code;
        llvm::raw_fd_ostream destination(outputFilename, error_code, llvm::sys::fs::F_None);

        llvm::legacy::PassManager pass;
        auto fileType = llvm::TargetMachine::CGFT_ObjectFile;

        if (targetMachine->addPassesToEmitFile(pass, destination, nullptr, fileType)) {
            llvm::errs() << "TheTargetMachine can't emit a file of this type";
            return false;
        }

        pass.run(*_module);
        destination.flush();
        return true;
    }
}