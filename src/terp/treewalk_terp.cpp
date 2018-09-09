#include "treewalk_terp.h"

#include <iostream>

#include "klong_function.h"

namespace klong {

#define getType(nameSuffix, anyVarName, typeVarName) \
    PrimitiveTypeKind kind##nameSuffix; \
    uint8_t u8##nameSuffix = 0; \
    uint16_t u16##nameSuffix = 0; \
    uint32_t u32##nameSuffix = 0; \
    uint64_t u64##nameSuffix = 0; \
    int8_t i8##nameSuffix = 0; \
    int16_t i16##nameSuffix = 0; \
    int32_t i32##nameSuffix = 0; \
    int64_t i64##nameSuffix = 0; \
    float f32##nameSuffix = 0.0f; \
    double f64##nameSuffix = 0.0f; \
    std::string str##nameSuffix; \
    bool boolean##nameSuffix = false; \
    if (typeVarName->kind() == TypeKind::PRIMITIVE) { \
        auto primType = dynamic_cast<PrimitiveType*>(typeVarName); \
        kind##nameSuffix = primType->type(); \
        switch (kind##nameSuffix) { \
            case PrimitiveTypeKind::U8: \
                u8##nameSuffix = std::any_cast<uint8_t>(anyVarName); \
                break; \
            case PrimitiveTypeKind::U16: \
                u16##nameSuffix = std::any_cast<uint16_t>(anyVarName); \
                break; \
            case PrimitiveTypeKind::U32: \
                u32##nameSuffix = std::any_cast<uint32_t>(anyVarName); \
                break; \
            case PrimitiveTypeKind::U64: \
                u64##nameSuffix = std::any_cast<uint32_t>(anyVarName); \
                break; \
            case PrimitiveTypeKind::I8: \
                i8##nameSuffix = std::any_cast<int8_t>(anyVarName); \
                break; \
            case PrimitiveTypeKind::I16: \
                i16##nameSuffix = std::any_cast<int16_t>(anyVarName); \
                break; \
            case PrimitiveTypeKind::I32: \
                i32##nameSuffix = std::any_cast<int32_t>(anyVarName); \
                break; \
            case PrimitiveTypeKind::I64: \
                i64##nameSuffix = std::any_cast<int64_t>(anyVarName); \
                break; \
            case PrimitiveTypeKind::F32: \
                f32##nameSuffix = std::any_cast<float>(anyVarName); \
                break; \
            case PrimitiveTypeKind::F64: \
                f64##nameSuffix = std::any_cast<double>(anyVarName); \
                break; \
            case PrimitiveTypeKind::STRING: \
                str##nameSuffix = std::any_cast<std::string>(anyVarName); \
                break; \
            case PrimitiveTypeKind::BOOL: \
                boolean##nameSuffix = std::any_cast<bool>(anyVarName); \
                break; \
            case PrimitiveTypeKind::VOID: \
                throw TerpException("Cannot extract from void type."); \
        } \
    } else { \
        throw TerpException("Cannot extract non primitive type from any."); \
    } \

#define runBinaryNumberOperation(op)      \
    do {                                  \
        if (isInteger(leftType)           \
            && isInteger(rightType)) {    \
            getType(l, left, leftType);   \
            getType(r, right, rightType); \
            if (isSigned(leftType)) {     \
                return i64l op i64r;      \
            }                             \
            return u64l op u64r;          \
        }                                 \
    } while(false)                        \

    std::any TreewalkTerp::evaluate(Expr* expr) {
        expr->accept(this);
        return _valueOfLastExpr;
    }

    void TreewalkTerp::execute(Stmt* stmt) {
        stmt->accept(this);
    }

    void TreewalkTerp::executeBlock(const std::vector<StmtPtr>& statements, std::shared_ptr<Environment> env) {
        std::shared_ptr<Environment> previous = this->_environment;
        try {
            this->_environment = env;
            // visit all functions of the current block first
            for (const auto& stmt : statements) {
                if (stmt->kind() == StatementKind::FUNCTION) {
                    execute(stmt.get());
                }
            }
            for (const auto& stmt : statements) {
                if (stmt->kind() == StatementKind::FUNCTION) {
                    continue;
                }
                execute(stmt.get());
            }
        } catch (...) {
            this->_environment = previous;
            throw;
        }
        this->_environment = previous;
    }

    bool TreewalkTerp::isTruthy(std::any value) {
        return std::any_cast<bool>(value);
    }

    bool TreewalkTerp::isFloat(Type* type) {
        if (type->kind() == TypeKind::PRIMITIVE) {
            auto primType = dynamic_cast<PrimitiveType*>(type);
            switch (primType->type()) {
                case PrimitiveTypeKind::F32:
                case PrimitiveTypeKind::F64:
                    return true;
                default:
                    return false;
            }
        }
        return false;
    }

    bool TreewalkTerp::isInteger(Type *type) {
        if (type->kind() == TypeKind::PRIMITIVE) {
            auto primType = dynamic_cast<PrimitiveType*>(type);
            switch (primType->type()) {
                case PrimitiveTypeKind::U8:
                case PrimitiveTypeKind::U16:
                case PrimitiveTypeKind::U32:
                case PrimitiveTypeKind::U64:
                case PrimitiveTypeKind::I8:
                case PrimitiveTypeKind::I16:
                case PrimitiveTypeKind::I32:
                case PrimitiveTypeKind::I64:
                    return true;
                default:
                    return false;
            }
        }
        return false;
    }

    bool TreewalkTerp::isSigned(Type* type) {
        if (type->kind() == TypeKind::PRIMITIVE) {
            auto primType = dynamic_cast<PrimitiveType*>(type);
            switch (primType->type()) {
                case PrimitiveTypeKind::U8:
                case PrimitiveTypeKind::U16:
                case PrimitiveTypeKind::U32:
                case PrimitiveTypeKind::U64:
                    return false;
                case PrimitiveTypeKind::I8:
                case PrimitiveTypeKind::I16:
                case PrimitiveTypeKind::I32:
                case PrimitiveTypeKind::I64:
                    return true;
                default:
                    break;
            }
        }
        throw TerpException("Checking signed on an non number type");
    }

    std::string TreewalkTerp::stringify(std::any value, Type* type) {
        getType(x, value, type);
        switch(kindx) {
            case PrimitiveTypeKind::STRING:
                return strx;
            case PrimitiveTypeKind::I8:
                return std::to_string(i8x);
            case PrimitiveTypeKind::I16:
                return std::to_string(i16x);
            case PrimitiveTypeKind::I32:
                return std::to_string(i32x);
            case PrimitiveTypeKind::I64:
                return std::to_string(i64x);
            case PrimitiveTypeKind::U8:
                return std::to_string(u8x);
            case PrimitiveTypeKind::U16:
                return std::to_string(u16x);
            case PrimitiveTypeKind::U32:
                return std::to_string(u32x);
            case PrimitiveTypeKind::U64:
                return std::to_string(u64x);
            case PrimitiveTypeKind::F32:
                return std::to_string(f32x);
            case PrimitiveTypeKind::F64:
                return std::to_string(f64x);
            case PrimitiveTypeKind::BOOL:
                return booleanx ? "true" : "false";
            default:
                throw TerpException("Cannot stringify void type.");
        }
        return std::any_cast<std::string>(value);
    }

    void TreewalkTerp::visitModule(Module* module) {
        executeBlock(module->statements(), std::make_shared<Environment>(_environment));
    }

    void TreewalkTerp::visitBlockStmt(Block* stmt) {
        executeBlock(stmt->statements(), std::make_shared<Environment>(_environment));
    }

    void TreewalkTerp::visitExpressionStmt(Expression* stmt) {
        evaluate(stmt->expression().get());
    }

    void TreewalkTerp::visitFunctionStmt(Function* stmt) {
        auto function = std::make_shared<KlongFunction>(stmt, _environment);
        _environment->define(stmt, function);
    }

    void TreewalkTerp::visitParameterStmt(Parameter* stmt) {
        // nothing to do here
    }

    void TreewalkTerp::visitIfStmt(If* stmt) {
        if (isTruthy(evaluate(stmt->condition().get()))) {
            execute(stmt->thenBranch().get());
        } else if (stmt->elseBranch() != nullptr) {
            execute(stmt->elseBranch().get());
        }
    }

    void TreewalkTerp::visitPrintStmt(Print* stmt) {
        std::any value = evaluate(stmt->expression().get());
        std::cout << stringify(value, stmt->expression()->type().get()) << std::flush;
    }

    void TreewalkTerp::visitReturnStmt(Return* stmt) {
        std::any value = nullptr;
        if (stmt->value() != nullptr) {
            value = evaluate(stmt->value().get());
        }

        throw ReturnWrapper(value);
    }

    void TreewalkTerp::visitLetStmt(Let* stmt) {
        std::any value = nullptr;
        if (stmt->initializer() != nullptr) {
            value = evaluate(stmt->initializer().get());
        }

        _environment->define(stmt, value);
    }

    void TreewalkTerp::visitConstStmt(Const* stmt) {
        std::any value = nullptr;
        if (stmt->initializer() != nullptr) {
            value = evaluate(stmt->initializer().get());
        }

        _environment->define(stmt, value);
    }

    void TreewalkTerp::visitWhileStmt(While* stmt) {
        while(isTruthy(evaluate(stmt->condition().get()))) {
            execute(stmt->body().get());
        }
    }

    void TreewalkTerp::visitForStmt(For* stmt) {
        for(execute(stmt->initializer().get());
            isTruthy(evaluate(stmt->condition().get()));
            evaluate(stmt->increment().get())) {
            execute(stmt->body().get());
        }
    }

    void TreewalkTerp::visitCommentStmt(Comment* stmt) {
        (void) stmt;
    }

    void TreewalkTerp::visitAssignExpr(Assign* expr) {
        std::any value = evaluate(expr->value().get());
        _environment->assign(expr->target()->resolvesTo(), value);
        _valueOfLastExpr = value;
    }

    std::any TreewalkTerp::isEqual(std::any left, Type* leftType, std::any right, Type* rightType) {
        runBinaryNumberOperation(==);
        throw TerpException("Illegal isEqual. Maybe the typechecker has a bug.");
    }

    std::any TreewalkTerp::isLessThan(std::any left, Type* leftType, std::any right, Type* rightType) {
        runBinaryNumberOperation(<);
        throw TerpException("Illegal isLessThan. Maybe the typechecker has a bug.");
    }

    std::any TreewalkTerp::isLessThanEqual(std::any left, Type* leftType, std::any right, Type* rightType) {
        runBinaryNumberOperation(<=);
        throw TerpException("Illegal isLessThanEqual. Maybe the typechecker has a bug.");
    }

    std::any TreewalkTerp::isGreaterThan(std::any left, Type* leftType, std::any right, Type* rightType) {
        runBinaryNumberOperation(>);
        throw TerpException("Illegal isGreaterThan. Maybe the typechecker has a bug.");
    }

    std::any TreewalkTerp::isGreaterThanEqual(std::any left, Type* leftType, std::any right, Type* rightType) {
        runBinaryNumberOperation(>=);
        throw TerpException("Illegal isGreaterThanEqual. Maybe the typechecker has a bug.");
    }

    std::any TreewalkTerp::minus(std::any left, Type* leftType, std::any right, Type* rightType) {
        runBinaryNumberOperation(-);
        throw TerpException("Illegal minus. Maybe the typechecker has a bug.");
    }

    std::any TreewalkTerp::plus(std::any left, Type* leftType, std::any right, Type* rightType) {
        runBinaryNumberOperation(+);
        auto strl = stringify(left, leftType);
        auto strr = stringify(right, rightType);
        return strl + strr;
    }

    std::any TreewalkTerp::multiplication(std::any left, Type* leftType, std::any right, Type* rightType) {
        runBinaryNumberOperation(*);
        throw TerpException("Illegal multiplication. Maybe the typechecker has a bug.");
    }

    std::any TreewalkTerp::division(std::any left, Type* leftType, std::any right, Type* rightType) {
        runBinaryNumberOperation(/);
        throw TerpException("Illegal division. Maybe the typechecker has a bug.");
    }

    void TreewalkTerp::visitBinaryExpr(Binary* expr) {
        std::any left = evaluate(expr->left().get());
        std::any right = evaluate(expr->right().get());
        switch(expr->op()) {
            case BinaryOperation::EQUALITY:
                _valueOfLastExpr = std::any_cast<bool>(
                        isEqual(left, expr->left()->type().get(), right, expr->right()->type().get()));
                return;
            case BinaryOperation::INEQUALITY:
                _valueOfLastExpr =
                        !std::any_cast<bool>(
                                isEqual(left, expr->left()->type().get(), right, expr->right()->type().get()));
                return;
            case BinaryOperation::LESS_THAN:
                _valueOfLastExpr = isLessThan(left, expr->left()->type().get(), right, expr->right()->type().get());
                return;
            case BinaryOperation::LESS_EQUAL:
                _valueOfLastExpr = isLessThanEqual(left, expr->left()->type().get(), right, expr->right()->type().get());
                return;
            case BinaryOperation::GREATER_THAN:
                _valueOfLastExpr = isGreaterThan(left, expr->left()->type().get(), right, expr->right()->type().get());
                return;
            case BinaryOperation::GREATER_EQUAL:
                _valueOfLastExpr = isGreaterThanEqual(left, expr->left()->type().get(), right, expr->right()->type().get());
                return;
            case BinaryOperation::MINUS:
                _valueOfLastExpr = minus(left, expr->left()->type().get(), right, expr->right()->type().get());
                return;
            case BinaryOperation::PLUS:
                _valueOfLastExpr = plus(left, expr->left()->type().get(), right, expr->right()->type().get());
                return;
            case BinaryOperation::MULTIPLICATION:
                _valueOfLastExpr = multiplication(left, expr->left()->type().get(), right, expr->right()->type().get());
                return;
            case BinaryOperation::DIVISION:
                _valueOfLastExpr = division(left, expr->left()->type().get(), right, expr->right()->type().get());
                return;
        }
    }

    void TreewalkTerp::visitCallExpr(Call* expr) {
        std::any callee = evaluate(expr->callee().get());

        std::vector<std::any> args;
        for (auto& argument : expr->args()) {
            args.push_back(evaluate(argument.get()));
        }
        auto function = std::any_cast<std::shared_ptr<KlongFunction>>(callee);
        _valueOfLastExpr = function->call(this, std::move(args));
    }

    void TreewalkTerp::visitGroupingExpr(Grouping* expr) {
        _valueOfLastExpr = evaluate(expr->expression().get());
    }

    void TreewalkTerp::visitLogicalExpr(Logical* expr) {
        std::any left = evaluate(expr->left().get());
        if (expr->op() == LogicalOperation::OR) {
            if (isTruthy(left)) {
                _valueOfLastExpr = left;
                return;
            }
        } else {
            if (!isTruthy(left)) {
                _valueOfLastExpr = left;
                return;
            }
        }

        _valueOfLastExpr = evaluate(expr->right().get());
    }

    void TreewalkTerp::visitUnaryExpr(Unary* expr) {
        std::any value = evaluate(expr->right().get());
        if (expr->operation() == UnaryOperation::NOT) {
            _valueOfLastExpr = !isTruthy(value);
            return;
        }

        if (expr->operation() == UnaryOperation::MINUS) {
            // TODO: fix this
            _valueOfLastExpr = -std::any_cast<int64_t>(value);
            return;
        }
    }

    void TreewalkTerp::visitVariableExpr(Variable* expr) {
        _valueOfLastExpr = _environment->get(expr->resolvesTo());
    }

    void TreewalkTerp::visitNumberLiteral(NumberLiteral* expr) {
        switch(expr->literalType()) {
            case PrimitiveTypeKind::I64:
                _valueOfLastExpr = expr->i64();
                break;
            case PrimitiveTypeKind::U64:
                _valueOfLastExpr = expr->u64();
                break;
            case PrimitiveTypeKind::F64:
                _valueOfLastExpr = expr->f64();
                break;
            default:
                break;
        }
    }

    void TreewalkTerp::visitBoolLiteral(BoolLiteral* expr) {
        _valueOfLastExpr = expr->value();
    }

    void TreewalkTerp::visitStringLiteral(StringLiteral* expr) {
        _valueOfLastExpr = expr->value();
    }

    void TreewalkTerp::visitCharacterLiteral(CharacterLiteral* expr) {
        _valueOfLastExpr = expr->value();
    }

    void TreewalkTerp::visitFunctionType(FunctionType* type) {
        (void) type;
    }

    void TreewalkTerp::visitPrimitiveType(PrimitiveType *type) {
        (void) type;
    }

    void TreewalkTerp::visitSimpleType(SimpleType *type) {
        (void) type;
    }

}