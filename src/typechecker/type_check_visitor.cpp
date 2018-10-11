#include <cassert>
#include <functional>

#include "type_check_visitor.h"

#include "resolver/resolve_visitor.h"
#include "ast/module.h"
#include "ast/stmt.h"
#include "ast/expr.h"
#include "ast/type.h"

namespace klong {

    void TypeCheckVisitor::check(const std::vector<StmtPtr>& statements) {
        for (const auto& stmt : statements) {
            check(stmt.get());
        }
    }

    void TypeCheckVisitor::check(Stmt* stmt) {
        if (stmt != nullptr) {
            stmt->accept(this);
        }
    }

    void TypeCheckVisitor::check(Expr* expr) {
        if (expr != nullptr) {
            expr->accept(this);
        }
    }

    bool TypeCheckVisitor::getAndResetReturnsValue() {
        auto returnsValue = _returnsValue;
        _returnsValue = false;
        return returnsValue;
    }

    bool TypeCheckVisitor::isBoolean(Expr* expr) {
        Type* type = expr->type().get();
        if (type && type->kind() == TypeKind::PRIMITIVE) {
            auto primitiveType = dynamic_cast<PrimitiveType*>(type);
            return primitiveType->isBoolean();
        }
        return false;
    }

    bool TypeCheckVisitor::isFloat(Expr* expr) {
        Type* type = expr->type().get();
        if (type && type->kind() == TypeKind::PRIMITIVE) {
            auto primitiveType = dynamic_cast<PrimitiveType*>(type);
            return primitiveType->isFloat();
        }
        return false;
    }

    bool TypeCheckVisitor::isInteger(Expr* expr) {
        Type* type = expr->type().get();
        if (type && type->kind() == TypeKind::PRIMITIVE) {
            auto primitiveType = dynamic_cast<PrimitiveType*>(type);
            return primitiveType->isInteger();
        }
        return false;
    }

    bool TypeCheckVisitor::isString(Expr* expr) {
        Type* type = expr->type().get();
        if (type && type->kind() == TypeKind::PRIMITIVE) {
            auto primitiveType = dynamic_cast<PrimitiveType*>(type);
            return primitiveType->isString();
        }
        return false;
    }

    bool TypeCheckVisitor::isPointer(Expr* expr) {
        Type* type = expr->type().get();
        if (type->kind() == TypeKind::POINTER) {
            auto pointerType = dynamic_cast<PointerType*>(type);
            return pointerType != nullptr;
        }
        return false;
    }

    // Module
    void TypeCheckVisitor::visitModule(Module* module) {
        check(module->statements());
    }

    // Stmt
    void TypeCheckVisitor::visitBlockStmt(Block* stmt) {
        check(stmt->statements());
    }

    void TypeCheckVisitor::visitExpressionStmt(Expression* stmt) {
        check(stmt->expression().get());
    }

    void TypeCheckVisitor::visitExtDeclStmt(ExternalDeclaration* stmt) {
        // nothing to do here
        (void) stmt;
    }

    void TypeCheckVisitor::visitFunctionStmt(Function* stmt) {
        auto previousFunction = currentFunction;
        currentFunction = stmt;
        check(stmt->body());
        auto primType = dynamic_cast<PrimitiveType*>(stmt->functionType()->returnType().get());
        if (!_returnsValue && primType != nullptr && !primType->isVoid()) {
            _result.addError(TypeCheckException(stmt->sourceRange(), "Control-flow reaches end of non-void function "
                + stmt->name() + "."));
        }
        currentFunction = previousFunction;
    }

    void TypeCheckVisitor::visitParameterStmt(Parameter* stmt) {
        // nothing to do here
        (void) stmt;
    }

    void TypeCheckVisitor::visitIfStmt(If* stmt) {
        check(stmt->condition().get());
        if (!isBoolean(stmt->condition().get())) {
            _result.addError(
                    TypeCheckException(stmt->condition()->sourceRange(), "Expect bool condition in if-statement."));
        }
        check(stmt->thenBranch().get());
        bool thenBranchReturnsValue = getAndResetReturnsValue();
        check(stmt->elseBranch().get());
        if (stmt->elseBranch().get() != nullptr) {
            _returnsValue = getAndResetReturnsValue() && thenBranchReturnsValue;
        } else {
            _returnsValue = false;
        }
        if (_returnsValue) {
            stmt->setMergeUnreachable();
        }
    }

    void TypeCheckVisitor::visitReturnStmt(Return* stmt) {
        check(stmt->value().get());
        if (stmt->value() != nullptr) {
            if (!currentFunction->functionType()->returnType()->isEqual(stmt->value()->type().get())) {
                _result.addError(
                        TypeCheckException(stmt->sourceRange(), "Expect return statement type to match the function returnType."));
            }
            _returnsValue = true;
        }
    }

    void TypeCheckVisitor::visitVarDeclStmt(VariableDeclaration* stmt) {
        if (currentFunction != nullptr && stmt->isPublic()) {
            _result.addError(
                    TypeCheckException(stmt->sourceRange(), "Pub keyword not allowed in front of local variable."));
        }
        check(stmt->initializer().get());
        if (stmt->type() == nullptr) {
            stmt->type(std::shared_ptr<Type>(stmt->initializer()->type()->clone()));
        } else {
            if (stmt->initializer() && !stmt->type()->isEqual(stmt->initializer()->type().get())) {
                _result.addError(
                        TypeCheckException(stmt->sourceRange(), "initializerType doesn't match declaration type."));
            }
        }
    }

    void TypeCheckVisitor::visitWhileStmt(While* stmt) {
        check(stmt->condition().get());
        if (!isBoolean(stmt->condition().get())) {
            _result.addError(
                    TypeCheckException(stmt->condition()->sourceRange(), "while condition expects bool type."));
        }
        check(stmt->body().get());
    }

    void TypeCheckVisitor::visitForStmt(For* stmt) {
        check(stmt->initializer().get());
        check(stmt->condition().get());
        if (!isBoolean(stmt->condition().get())) {
            _result.addError(
                    TypeCheckException(stmt->condition()->sourceRange(), "for condition expects bool type."));
        }
        check(stmt->increment().get());
        check(stmt->body().get());
    }

    void TypeCheckVisitor::visitCommentStmt(Comment* stmt) {
        // empty on purpose
        (void) stmt;
    }

    // Expr
    void TypeCheckVisitor::visitAssignExpr(Assign* expr) {
        check(expr->target().get());
        check(expr->targetDeref().get());
        check(expr->value().get());

        Type* targetType = nullptr;

        if (expr->isTargetVariable()) {
            targetType = expr->target()->type().get();
        } else {
            targetType = expr->targetDeref()->type().get();
        }

        if (!targetType->isEqual(expr->value()->type().get())) {
            _result.addError(
                    TypeCheckException(expr->value()->sourceRange(), "Expect valid type in assignment."));
        }

        if (expr->value()->type()) {
            expr->type(std::shared_ptr<Type>(expr->value()->type()->clone()));
        }
    }

    void TypeCheckVisitor::visitBinaryExpr(Binary* expr) {
        check(expr->left().get());
        check(expr->right().get());
        // TODO: primitive type hierarchie
        if (isInteger(expr->left().get()) && isInteger(expr->right().get())) {
            switch(expr->op()) {
                case BinaryOperation::PLUS:
                case BinaryOperation::MINUS:
                case BinaryOperation::MULTIPLICATION:
                case BinaryOperation::DIVISION:
                case BinaryOperation::MODULO:
                case BinaryOperation::LSL:
                case BinaryOperation::LSR:
                case BinaryOperation::ASR:
                case BinaryOperation::AND:
                case BinaryOperation::XOR:
                case BinaryOperation::OR:
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

        if (isFloat(expr->left().get()) && isFloat(expr->right().get())) {
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
                default:
                    _result.addError(
                            TypeCheckException(expr->sourceRange(), "Illegal operation for floating number."));
            }
            return;
        }

        if (expr->op() == BinaryOperation::PLUS) {
            std::function<bool(Expr*)> isValidOtherType = [this](Expr* exprPtr) {
                return isFloat(exprPtr) || isInteger(exprPtr) || isBoolean(exprPtr);
            };
            if ((isString(expr->left().get()) && isValidOtherType(expr->right().get()))
                || (isValidOtherType(expr->left().get()) && isString(expr->right().get()))
                || (isString(expr->left().get()) && isString(expr->right().get()))) {
                expr->type(std::make_shared<PrimitiveType>(SourceRange(), PrimitiveTypeKind::STRING));
                return;
            }
        }
        _result.addError(
                TypeCheckException(expr->sourceRange(), "Illegal type in binary op."));
    }

    void TypeCheckVisitor::visitCallExpr(Call* expr) {
        check(expr->callee().get());
        std::vector<TypePtr> callParamTypes;
        for (const auto& arg : expr->args()) {
            check(arg.get());
            callParamTypes.push_back(arg->type());
        }
        auto calleeType = expr->callee()->type();
        if (calleeType->kind() == TypeKind::POINTER) {
            auto calleePointer = dynamic_cast<PointerType*>(calleeType.get());
            auto functionType = dynamic_cast<FunctionType*>(calleePointer->pointsTo().get());
            if (functionType != nullptr) {
                if (!functionType->matchesSignature(callParamTypes)) {
                    _result.addError(
                            TypeCheckException(expr->sourceRange(), "Call Expr doesn't match function signature."));
                }
                expr->type(std::shared_ptr<Type>(functionType->returnType()->clone()));
                return;
            }
        }
        _result.addError(
                TypeCheckException(expr->sourceRange(), "Callee doesn't resolve to function pointer expression."));
    }

    void TypeCheckVisitor::visitGroupingExpr(Grouping* expr) {
        check(expr->expression().get());
        expr->type(expr->expression()->type());
    }

    void TypeCheckVisitor::visitLogicalExpr(Logical* expr) {
        check(expr->left().get());
        if (!isBoolean(expr->left().get())) {
            _result.addError(
                    TypeCheckException(expr->left()->sourceRange(), "Expect boolean expr."));
        }
        check(expr->right().get());
        if (!isBoolean(expr->right().get())) {
            _result.addError(
                    TypeCheckException(expr->right()->sourceRange(), "Expect boolean expr."));
        }
        expr->type(std::make_shared<PrimitiveType>(SourceRange(), PrimitiveTypeKind::BOOL));
    }

    void TypeCheckVisitor::visitUnaryExpr(Unary* expr) {
        check(expr->right().get());

        if (expr->op() == UnaryOperation::NOT && !isBoolean(expr->right().get())) {
            _result.addError(
                    TypeCheckException(expr->sourceRange(), "'!' expects boolean expression."));
        }

        if (expr->op() == UnaryOperation::MINUS && !isInteger(expr->right().get())) {
            _result.addError(
                    TypeCheckException(expr->sourceRange(), "Unary '-' expects number expression."));
        }

        if (expr->op() == UnaryOperation::DEREF) {
            if (!isPointer(expr->right().get())) {
                _result.addError(
                        TypeCheckException(expr->sourceRange(), "Deref expects pointer type."));
                return;
            }
            auto pointerType = dynamic_cast<PointerType*>(expr->right()->type().get());
            if (pointerType->pointsTo()->kind() == TypeKind::FUNCTION) {
                _result.addError(
                        TypeCheckException(expr->sourceRange(), "Deref expects non function pointer type."));
            }
            expr->type(std::shared_ptr<Type>(pointerType->pointsTo()->clone()));
            return;
        }

        if (expr->op() == UnaryOperation::ADDRESS_OF) {
            auto variable = dynamic_cast<Variable*>(expr->right().get());
            if (variable == nullptr) {
                _result.addError(
                        TypeCheckException(expr->sourceRange(), "Can only get address of variable expressions."));
            } else {
                auto isFunction = false;
                if (variable->type()->kind() == TypeKind::FUNCTION) {
                    isFunction = true;
                }

                if (variable->type()->kind() == TypeKind::POINTER) {
                    auto pointerType = dynamic_cast<PointerType*>(variable->type().get());
                    if (pointerType->pointsTo()->kind() == TypeKind::FUNCTION) {
                        isFunction = true;
                    }
                }

                if (isFunction) {
                    _result.addError(
                            TypeCheckException(expr->sourceRange(),
                                               "Can not get address of function. Function names are already pointers."));
                }
            }
            expr->type(std::make_shared<PointerType>(
                    expr->sourceRange(),
                    std::shared_ptr<Type>(expr->right()->type()->clone())));
            return;
        }
        expr->type(std::shared_ptr<Type>(expr->right()->type()->clone()));
    }

    void TypeCheckVisitor::visitVariableExpr(Variable* expr) {
        Stmt* resolvesTo = expr->resolvesTo();
        switch(resolvesTo->kind()) {
            case StatementKind::FUNCTION:
            {
                auto function = dynamic_cast<Function*>(resolvesTo);
                auto clonedFunction = std::shared_ptr<Type>(function->functionType()->clone());
                auto pointerToClonedFunction = std::make_shared<PointerType>(clonedFunction->sourceRange(), clonedFunction);
                expr->type(pointerToClonedFunction);
                break;
            }
            case StatementKind::EXT_DECL:
            {
                auto extDecl = dynamic_cast<ExternalDeclaration*>(resolvesTo);
                auto clonedType = std::shared_ptr<Type>(extDecl->type()->clone());
                if (clonedType->kind() == TypeKind::FUNCTION) {
                    clonedType = std::make_shared<PointerType>(clonedType->sourceRange(), clonedType);
                }
                expr->type(clonedType);
                break;
            }
            case StatementKind::VAR_DECL:
            {
                auto varDecl = dynamic_cast<VariableDeclaration*>(resolvesTo);
                expr->type(std::shared_ptr<Type>(varDecl->type()->clone()));
                break;
            }
            case StatementKind::PARAMETER:
            {
                auto paramStmt = dynamic_cast<Parameter*>(resolvesTo);
                expr->type(std::shared_ptr<Type>(paramStmt->type()->clone()));
                break;
            }
            default:
                _result.addError(
                        TypeCheckException(expr->sourceRange(), "Variable resolves to invalid kind."));
        }
    }

    // Literals
    void TypeCheckVisitor::visitNumberLiteral(NumberLiteral* expr) {
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

    void TypeCheckVisitor::visitBoolLiteral(BoolLiteral* expr) {
        TypePtr type = std::make_shared<PrimitiveType>(expr->sourceRange(), PrimitiveTypeKind::BOOL);
        expr->type(type);
    }

    void TypeCheckVisitor::visitStringLiteral(StringLiteral* expr) {
        TypePtr type = std::make_shared<PrimitiveType>(expr->sourceRange(), PrimitiveTypeKind::STRING);
        expr->type(type);
    }

    void TypeCheckVisitor::visitCharacterLiteral(CharacterLiteral* expr) {
        TypePtr type = std::make_shared<PrimitiveType>(expr->sourceRange(), PrimitiveTypeKind::I8);
        expr->type(type);
    }

    Result<ModulePtr, TypeCheckException> TypeCheckVisitor::getResult() const {
        return _result;
    }
}
