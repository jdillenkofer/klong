#include <functional>

#include "type_checker.h"

#include "resolver.h"
#include "module.h"
#include "stmt.h"
#include "expr.h"
#include "type.h"

namespace klong {

    void TypeChecker::check(const std::vector<StmtPtr>& statements) {
        for (const auto& stmt : statements) {
            check(stmt.get());
        }
    }

    void TypeChecker::check(Stmt* stmt) {
        if (stmt != nullptr) {
            stmt->accept(this);
        }
    }

    void TypeChecker::check(Expr* expr) {
        if (expr != nullptr) {
            expr->accept(this);
        }
    }

    bool TypeChecker::isBoolean(Expr* expr) {
        TypePtr type = expr->type();
        if (type->kind() == TypeKind::PRIMITIVE) {
            auto primitiveType = std::dynamic_pointer_cast<PrimitiveType>(type);
            if (primitiveType->type() == PrimitiveTypeKind::BOOL) {
                return true;
            }
        }
        return false;
    }

    bool TypeChecker::isFloat(Expr* expr) {
        TypePtr type = expr->type();
        if (type->kind() == TypeKind::PRIMITIVE) {
            auto primitiveType = std::dynamic_pointer_cast<PrimitiveType>(type);
            switch(primitiveType->type()) {
                case PrimitiveTypeKind::F32:
                case PrimitiveTypeKind::F64:
                    return true;
                default:
                    break;
            }
        }
        return false;
    }

    bool TypeChecker::isInteger(Expr* expr) {
        TypePtr type = expr->type();
        if (type->kind() == TypeKind::PRIMITIVE) {
            auto primitiveType = std::dynamic_pointer_cast<PrimitiveType>(type);
            switch(primitiveType->type()) {
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
                    break;
            }
        }
        return false;
    }

    bool TypeChecker::isString(Expr* expr) {
        TypePtr type = expr->type();
        if (type->kind() == TypeKind::PRIMITIVE) {
            auto primitiveType = std::dynamic_pointer_cast<PrimitiveType>(type);
            switch(primitiveType->type()) {
                case PrimitiveTypeKind::STRING:
                    return true;
                default:
                    break;
            }
        }
        return false;
    }

    // Module
    void TypeChecker::visitModule(Module* module) {
        check(module->statements());
    }

    // Stmt
    void TypeChecker::visitBlockStmt(Block* stmt) {
        check(stmt->statements());
    }

    void TypeChecker::visitExpressionStmt(Expression* stmt) {
        check(stmt->expression().get());
    }

    void TypeChecker::visitFunctionStmt(Function* stmt) {
        auto previousFunction = currentFunction;
        currentFunction = stmt;
        check(stmt->body());
        currentFunction = previousFunction;
    }

    void TypeChecker::visitParameterStmt(Parameter* stmt) {
        // nothing to do here
        (void) stmt;
    }

    void TypeChecker::visitIfStmt(If* stmt) {
        check(stmt->condition().get());
        if (!isBoolean(stmt->condition().get())) {
            throw TypeCheckException(stmt->condition()->sourceRange(), "Expect bool condition in if-statement.");
        }
        check(stmt->thenBranch().get());
        check(stmt->elseBranch().get());
    }

    void TypeChecker::visitPrintStmt(Print* stmt) {
        Expr* expr = stmt->expression().get();
        check(expr);
        if (!isString(expr) && !isInteger(expr) && !isFloat(expr) && !isBoolean(expr)) {
            throw TypeCheckException(stmt->sourceRange(), "Expect string, integer, float or boolean type in print stmt.");
        }
    }

    void TypeChecker::visitReturnStmt(Return* stmt) {
        check(stmt->value().get());
        if (stmt->value() != nullptr) {
            if (!currentFunction->functionType()->returnType()->isEqual(stmt->value()->type().get())) {
                throw TypeCheckException(stmt->sourceRange(), "Expect return statement type to match the function returnType.");
            }
        }
    }

    void TypeChecker::visitLetStmt(Let* stmt) {
        check(stmt->initializer().get());
        if (stmt->type() == nullptr) {
            stmt->type(std::shared_ptr<Type>(stmt->initializer()->type()->clone()));
        } else {
            if (stmt->initializer() && !stmt->type()->isEqual(stmt->initializer()->type().get())) {
                throw TypeCheckException(stmt->sourceRange(), "initializerType doesn't match let type.");
            }
        }
    }

    void TypeChecker::visitConstStmt(Const* stmt) {
        check(stmt->initializer().get());
        if (stmt->type() == nullptr) {
            stmt->type(std::shared_ptr<Type>(stmt->initializer()->type()->clone()));
        } else {
            if (!stmt->type()->isEqual(stmt->initializer()->type().get())) {
                throw TypeCheckException(stmt->sourceRange(), "initializerType doesn't match const type.");
            }
        }
    }

    void TypeChecker::visitWhileStmt(While* stmt) {
        check(stmt->condition().get());
        if (!isBoolean(stmt->condition().get())) {
            throw TypeCheckException(stmt->condition()->sourceRange(), "while condition expects bool type.");
        }
        check(stmt->body().get());
    }

    void TypeChecker::visitForStmt(For* stmt) {
        check(stmt->initializer().get());
        check(stmt->condition().get());
        if (!isBoolean(stmt->condition().get())) {
            throw TypeCheckException(stmt->condition()->sourceRange(), "for condition expects bool type.");
        }
        check(stmt->increment().get());
        check(stmt->body().get());
    }

    void TypeChecker::visitCommentStmt(Comment* stmt) {
        // empty on purpose
        (void) stmt;
    }

    // Expr
    void TypeChecker::visitAssignExpr(Assign* expr) {
        check(expr->target().get());
        check(expr->value().get());
        if (!expr->target()->type()->isEqual(expr->value()->type().get())) {
            throw TypeCheckException(expr->value()->sourceRange(), "Expect valid type in assignment.");
        }
        expr->type(std::shared_ptr<Type>(expr->value()->type()->clone()));
    }

    void TypeChecker::visitBinaryExpr(Binary* expr) {
        check(expr->left().get());
        check(expr->right().get());
        // TODO: doubles and primitive type hierarchie
        if (isInteger(expr->left().get()) && isInteger(expr->right().get())) {
            switch(expr->op()) {
                case BinaryOperation::PLUS:
                case BinaryOperation::MINUS:
                case BinaryOperation::MULTIPLICATION:
                case BinaryOperation::DIVISION:
                {
                    // TODO: cast to biggest number type
                    expr->type(std::shared_ptr<Type>(expr->left()->type()->clone()));
                    break;
                }
                case BinaryOperation::LESS_THAN:
                case BinaryOperation::LESS_EQUAL:
                case BinaryOperation::EQUALITY:
                case BinaryOperation::INEQUALITY:
                case BinaryOperation::GREATER_THAN:
                case BinaryOperation::GREATER_EQUAL:
                {
                    expr->type(std::make_shared<PrimitiveType>(SourceRange(), PrimitiveTypeKind::BOOL));
                    break;
                }
            }
            return;
        }

        if (expr->op() == BinaryOperation::PLUS) {
            std::function<bool(Expr*)> isValidOtherType = [this](Expr* exprPtr) {
                return isInteger(exprPtr) || isBoolean(exprPtr);
            };
            if ((isString(expr->left().get()) && isValidOtherType(expr->right().get()))
                || (isValidOtherType(expr->left().get()) && isString(expr->right().get()))
                || (isString(expr->left().get()) && isString(expr->right().get()))) {
                expr->type(std::make_shared<PrimitiveType>(SourceRange(), PrimitiveTypeKind::STRING));
                return;
            }
        }
        throw TypeCheckException(expr->sourceRange(), "Illegal type in binary operation.");
    }

    void TypeChecker::visitCallExpr(Call* expr) {
        check(expr->callee().get());
        std::vector<TypePtr> callParamTypes;
        for (const auto& arg : expr->args()) {
            check(arg.get());
            callParamTypes.push_back(arg->type());
        }
        if (expr->callee()->type()->kind() == TypeKind::FUNCTION) {
            auto functionType = std::dynamic_pointer_cast<FunctionType>(expr->callee()->type());
            if (!functionType->matchesSignature(callParamTypes)) {
                throw TypeCheckException(expr->sourceRange(), "Call Expr doesn't match function signature.");
            }
            expr->type(std::shared_ptr<Type>(functionType->returnType()->clone()));
            return;
        }
        throw TypeCheckException(expr->sourceRange(), "Callee doesn't resolve to function expression.");
    }

    void TypeChecker::visitGroupingExpr(Grouping* expr) {
        check(expr->expression().get());
        expr->type(expr->expression()->type());
    }

    void TypeChecker::visitLogicalExpr(Logical* expr) {
        check(expr->left().get());
        if (!isBoolean(expr->left().get())) {
            throw TypeCheckException(expr->left()->sourceRange(), "Expect boolean expr.");
        }
        check(expr->right().get());
        if (!isBoolean(expr->right().get())) {
            throw TypeCheckException(expr->right()->sourceRange(), "Expect boolean expr.");
        }
        expr->type(std::make_shared<PrimitiveType>(SourceRange(), PrimitiveTypeKind::BOOL));
    }

    void TypeChecker::visitUnaryExpr(Unary* expr) {
        check(expr->right().get());
        if (expr->operation() == UnaryOperation::NOT && !isBoolean(expr->right().get())) {
            throw TypeCheckException(expr->sourceRange(), "'!' expects boolean expression.");
        }
        if (expr->operation() == UnaryOperation::MINUS && !isInteger(expr->right().get())) {
            throw TypeCheckException(expr->sourceRange(), "Unary '-' expects number expression.");
        }
        expr->type(std::shared_ptr<Type>(expr->right()->type()->clone()));
    }

    void TypeChecker::visitVariableExpr(Variable* expr) {
        Stmt* resolvesTo = expr->resolvesTo();
        switch(resolvesTo->kind()) {
            case StatementKind::FUNCTION:
            {
                auto function = dynamic_cast<Function*>(resolvesTo);
                expr->type(std::shared_ptr<Type>(function->functionType()->clone()));
                break;
            }
            case StatementKind::CONST:
            {
                auto constStmt = dynamic_cast<Const*>(resolvesTo);
                expr->type(std::shared_ptr<Type>(constStmt->type()->clone()));
                break;
            }
            case StatementKind::LET:
            {
                auto letStmt = dynamic_cast<Let*>(resolvesTo);
                expr->type(std::shared_ptr<Type>(letStmt->type()->clone()));
                break;
            }
            case StatementKind::PARAMETER:
            {
                auto paramStmt = dynamic_cast<Parameter*>(resolvesTo);
                expr->type(std::shared_ptr<Type>(paramStmt->type()->clone()));
                break;
            }
            default:
                throw TypeCheckException(expr->sourceRange(), "Variable resolves to invalid kind.");
        }
    }

    // Literals
    void TypeChecker::visitNumberLiteral(NumberLiteral* expr) {
        TypePtr type;
        switch(expr->literalType()) {
            case PrimitiveTypeKind::I64:
                type = std::make_shared<PrimitiveType>(expr->sourceRange(), PrimitiveTypeKind::I64);
                break;
            case PrimitiveTypeKind::U64:
                type = std::make_shared<PrimitiveType>(expr->sourceRange(), PrimitiveTypeKind::U64);
                break;
            case PrimitiveTypeKind::F64:
                type = std::make_shared<PrimitiveType>(expr->sourceRange(), PrimitiveTypeKind::F64);
                break;
            default:
                break;
        }
        expr->type(type);
    }

    void TypeChecker::visitBoolLiteral(BoolLiteral* expr) {
        TypePtr type = std::make_shared<PrimitiveType>(expr->sourceRange(), PrimitiveTypeKind::BOOL);
        expr->type(type);
    }

    void TypeChecker::visitStringLiteral(StringLiteral* expr) {
        TypePtr type = std::make_shared<PrimitiveType>(expr->sourceRange(), PrimitiveTypeKind::STRING);
        expr->type(type);
    }

    void TypeChecker::visitCharacterLiteral(CharacterLiteral* expr) {
        TypePtr type = std::make_shared<PrimitiveType>(expr->sourceRange(), PrimitiveTypeKind::I8);
        expr->type(type);
    }

    // Types
    void TypeChecker::visitFunctionType(FunctionType* type) {
        // nothing to do here
        (void) type;
    }

    void TypeChecker::visitPrimitiveType(PrimitiveType *type) {
        // nothing to do here
        (void) type;
    }

    void TypeChecker::visitSimpleType(SimpleType *type) {
        // nothing to do here
        (void) type;
    }
}
