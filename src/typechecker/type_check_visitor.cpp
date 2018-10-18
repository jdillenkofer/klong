#include <cassert>
#include <functional>

#include "type_check_visitor.h"

#include "resolver/resolve_visitor.h"
#include "ast/module.h"
#include "ast/stmt.h"
#include "ast/expr.h"
#include "ast/type.h"

namespace klong {

    void TypeCheckVisitor::check(const std::vector<Stmt*>& statements) {
        for (const auto& stmt : statements) {
            check(stmt);
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

    TypePtr TypeCheckVisitor::applyIntegerPromotion(Type* type) {
        auto primitiveType = dynamic_cast<PrimitiveType*>(type);
        if (primitiveType && primitiveType->isInteger()) {
            if (primitiveType->isSigned()) {
                return std::make_shared<PrimitiveType>(type->sourceRange(), PrimitiveTypeKind::I64);
            } else {
                return std::make_shared<PrimitiveType>(type->sourceRange(), PrimitiveTypeKind::U64);
            }
        }
        if (primitiveType && primitiveType->isFloat()) {
            return std::make_shared<PrimitiveType>(type->sourceRange(), PrimitiveTypeKind::F64);
        }
        return nullptr;
    }

    TypePtr TypeCheckVisitor::applyArithmeticPromotion(Type* left, Type* right) {
        size_t leftIndex = 0;
        size_t rightIndex = 0;
        for (auto& type : _arithmeticConversionStack) {
            if (type->isEqual(left)) {
                break;
            }
            leftIndex++;
        }

        for (auto& type : _arithmeticConversionStack) {
            if (type->isEqual(right)) {
                break;
            }
            rightIndex++;
        }
        if (leftIndex > rightIndex) {
            return _arithmeticConversionStack[leftIndex];
        } else {
            return _arithmeticConversionStack[rightIndex];
        }
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
        check(stmt->expression());
    }

    void TypeCheckVisitor::visitExtDeclStmt(ExternalDeclaration* stmt) {
        // nothing to do here
        (void) stmt;
    }

    void TypeCheckVisitor::visitFunctionStmt(Function* stmt) {
        auto previousFunction = currentFunction;
        currentFunction = stmt;
        for (auto& param : stmt->params()) {
            check(param);
        }
        check(stmt->body());
        auto primType = dynamic_cast<PrimitiveType*>(stmt->functionType()->returnType());
        if (!_returnsValue && primType != nullptr && !primType->isVoid()) {
            _result.addError(
                    TypeCheckException(stmt->sourceRange(),
                            "Control-flow reaches end of non-void function "
                            + stmt->name() + "."));
        }
        currentFunction = previousFunction;
    }

    void TypeCheckVisitor::visitParameterStmt(Parameter* stmt) {
        if (stmt->type()->kind() == TypeKind::FUNCTION) {
            _result.addError(TypeCheckException(stmt->sourceRange(),
                    "Parameters of type functionType are not allowed."));
        }
    }

    void TypeCheckVisitor::visitIfStmt(If* stmt) {
        check(stmt->condition());
        if (!Type::isBoolean(stmt->condition()->type())) {
            _result.addError(
                    TypeCheckException(stmt->condition()->sourceRange(), "Expect bool condition in if-statement."));
        }
        check(stmt->thenBranch());
        bool thenBranchReturnsValue = getAndResetReturnsValue();
        check(stmt->elseBranch());
        if (stmt->elseBranch() != nullptr) {
            _returnsValue = getAndResetReturnsValue() && thenBranchReturnsValue;
        } else {
            _returnsValue = false;
        }
        if (_returnsValue) {
			stmt->setMergeUnreachable();
        }
    }

    void TypeCheckVisitor::visitReturnStmt(Return* stmt) {
        check(stmt->value());
        if (stmt->value() != nullptr) {
            if (!currentFunction->functionType()->returnType()->isEqual(stmt->value()->type())) {
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
        check(stmt->initializer());
        if (stmt->type() == nullptr) {
            stmt->type(std::shared_ptr<Type>(stmt->initializer()->type()->clone()));
        } else {
            if (stmt->initializer() && !stmt->type()->isEqual(stmt->initializer()->type())) {
                _result.addError(
                        TypeCheckException(stmt->sourceRange(), "initializerType doesn't match declaration type."));
            }
        }
    }

    void TypeCheckVisitor::visitWhileStmt(While* stmt) {
        check(stmt->condition());
        if (!Type::isBoolean(stmt->condition()->type())) {
            _result.addError(
                    TypeCheckException(stmt->condition()->sourceRange(), "while condition expects bool type."));
        }
        check(stmt->body());
    }

    void TypeCheckVisitor::visitForStmt(For* stmt) {
        check(stmt->initializer());
        check(stmt->condition());
        if (!Type::isBoolean(stmt->condition()->type())) {
            _result.addError(
                    TypeCheckException(stmt->condition()->sourceRange(), "for condition expects bool type."));
        }
        check(stmt->increment());
        check(stmt->body());
    }

    void TypeCheckVisitor::visitCommentStmt(Comment* stmt) {
        // empty on purpose
        (void) stmt;
    }

    // Expr
    void TypeCheckVisitor::visitAssignExpr(Assign* expr) {
        check(expr->target());
        check(expr->targetDeref());
        check(expr->value());

        Type* targetType = nullptr;

        if (expr->isTargetVariable()) {
            targetType = expr->target()->type();
        } else {
            targetType = expr->targetDeref()->type();
        }

        if (!targetType->isEqual(expr->value()->type())) {
            _result.addError(
                    TypeCheckException(expr->value()->sourceRange(), "Expect valid type in assignment."));
        }

        if (expr->value()->type()) {
            expr->type(std::shared_ptr<Type>(expr->value()->type()->clone()));
        }
    }

    void TypeCheckVisitor::visitBinaryExpr(Binary* expr) {
        check(expr->left());
        check(expr->right());
        auto leftType = expr->left()->type();
        auto rightType = expr->right()->type();
        auto resultType = std::shared_ptr<Type>(expr->left()->type()->clone());

        if (!Type::isInteger(leftType) || !Type::isInteger(rightType)) {
            switch(expr->op()) {
                case BinaryOperation::MODULO:
                case BinaryOperation::LSL:
                case BinaryOperation::LSR:
                case BinaryOperation::ASR:
                case BinaryOperation::AND:
                case BinaryOperation::XOR:
                case BinaryOperation::OR:
                    _result.addError(
                            TypeCheckException(expr->sourceRange(),
                                    "Illegal Operation"));
                    expr->type(std::make_shared<PrimitiveType>(PrimitiveTypeKind::I64));
                    return;
                default:
                    break;
            }
        }

        if ((Type::isInteger(leftType) && Type::isInteger(rightType))
            || (Type::isFloat(leftType) && Type::isFloat(rightType))) {
            switch (expr->op()) {
                case BinaryOperation::LESS_THAN:
                case BinaryOperation::LESS_EQUAL:
                case BinaryOperation::EQUALITY:
                case BinaryOperation::INEQUALITY:
                case BinaryOperation::GREATER_THAN:
                case BinaryOperation::GREATER_EQUAL:
                {
                    auto promotedLeftType = applyIntegerPromotion(leftType);
                    auto promotedRightType = applyIntegerPromotion(rightType);
                    if (promotedLeftType && promotedRightType &&
                        promotedLeftType->isEqual(promotedRightType.get())) {
                        resultType = promotedLeftType;
                    }
                    expr->castToType(resultType);
                    expr->type(std::make_shared<PrimitiveType>(PrimitiveTypeKind::BOOL));
                    return;
                }
                default:
                    break;
            }
        }
        if ((Type::isInteger(leftType) || Type::isFloat(leftType))
            && (Type::isInteger(rightType) || Type::isFloat(rightType))) {
            if (!leftType->isEqual(rightType)) {
                auto promotedLeftType = applyIntegerPromotion(leftType);
                auto promotedRightType = applyIntegerPromotion(rightType);
                if (promotedLeftType && promotedRightType &&
                    promotedLeftType->isEqual(promotedRightType.get())) {
                    resultType = promotedLeftType;
                } else {
                    // arithmetic promotion
                    auto arithmeticPromotedType = applyArithmeticPromotion(leftType, rightType);
                    resultType = arithmeticPromotedType;
                }
            }
            switch (expr->op()) {
                case BinaryOperation::LESS_THAN:
                case BinaryOperation::LESS_EQUAL:
                case BinaryOperation::EQUALITY:
                case BinaryOperation::INEQUALITY:
                case BinaryOperation::GREATER_THAN:
                case BinaryOperation::GREATER_EQUAL:
                    _result.addError(TypeCheckException(expr->sourceRange(),
                            "Comparisons must be of the same type."));
                    break;
                default:
                {
                    expr->castToType(resultType);
                    expr->type(resultType);
                    break;
                }
            }
            return;
        }
        if (Type::isPointer(leftType) && Type::isInteger(rightType)) {
            switch (expr->op()) {
                case BinaryOperation::PLUS:
                case BinaryOperation::MINUS:
                {
                    expr->type(std::shared_ptr<Type>(leftType->clone()));
                    return;
                }
                default:
                    break;
            }
        }
        if (Type::isPointer(leftType) && Type::isPointer(rightType)) {
            switch (expr->op()) {
                case BinaryOperation::LESS_THAN:
                case BinaryOperation::LESS_EQUAL:
                case BinaryOperation::EQUALITY:
                case BinaryOperation::INEQUALITY:
                case BinaryOperation::GREATER_THAN:
                case BinaryOperation::GREATER_EQUAL:
                {
                    expr->type(std::make_shared<PrimitiveType>(PrimitiveTypeKind::BOOL));
                    return;
                }
                default:
                    break;
            }
        }
        _result.addError(
                TypeCheckException(expr->sourceRange(),
                        "Illegal type in binary op."));
    }

    void TypeCheckVisitor::visitCallExpr(Call* expr) {
        check(expr->callee());
        std::vector<Type*> callParamTypes;
        for (const auto& arg : expr->args()) {
            check(arg);
            callParamTypes.push_back(arg->type());
        }
        auto calleeType = expr->callee()->type();
        if (calleeType->kind() == TypeKind::POINTER) {
            auto calleePointer = dynamic_cast<PointerType*>(calleeType);
            auto functionType = dynamic_cast<FunctionType*>(calleePointer->pointsTo());
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
        check(expr->expression());
        expr->type(std::shared_ptr<Type>(expr->expression()->type()->clone()));
    }

    void TypeCheckVisitor::visitLogicalExpr(Logical* expr) {
        check(expr->left());
        if (!Type::isBoolean(expr->left()->type())) {
            _result.addError(
                    TypeCheckException(expr->left()->sourceRange(), "Expect boolean expr."));
        }
        check(expr->right());
        if (!Type::isBoolean(expr->right()->type())) {
            _result.addError(
                    TypeCheckException(expr->right()->sourceRange(), "Expect boolean expr."));
        }
        expr->type(std::make_shared<PrimitiveType>(PrimitiveTypeKind::BOOL));
    }

    void TypeCheckVisitor::visitUnaryExpr(Unary* expr) {
        check(expr->right());

        if (expr->op() == UnaryOperation::NOT && !Type::isBoolean(expr->right()->type())) {
            _result.addError(
                    TypeCheckException(expr->sourceRange(), "'!' expects boolean expression."));
        }

        if (expr->op() == UnaryOperation::MINUS && !Type::isInteger(expr->right()->type())) {
            _result.addError(
                    TypeCheckException(expr->sourceRange(), "Unary '-' expects number expression."));
        }

        if (expr->op() == UnaryOperation::DEREF) {
            if (!Type::isPointer(expr->right()->type())) {
                _result.addError(
                        TypeCheckException(expr->sourceRange(), "Deref expects pointer type."));
                return;
            }
            auto pointerType = dynamic_cast<PointerType*>(expr->right()->type());
            if (pointerType->pointsTo()->kind() == TypeKind::FUNCTION) {
                _result.addError(
                        TypeCheckException(expr->sourceRange(), "Deref expects non function pointer type."));
            }
            expr->type(std::shared_ptr<Type>(pointerType->pointsTo()->clone()));
            return;
        }

        if (expr->op() == UnaryOperation::ADDRESS_OF) {
            auto variable = dynamic_cast<Variable*>(expr->right());
            if (variable == nullptr) {
                _result.addError(
                        TypeCheckException(expr->sourceRange(), "Can only get address of variable expressions."));
            } else {
                auto isFunction = false;
                if (variable->type()->kind() == TypeKind::FUNCTION) {
                    isFunction = true;
                }

                if (variable->type()->kind() == TypeKind::POINTER) {
                    auto pointerType = dynamic_cast<PointerType*>(variable->type());
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

    void TypeCheckVisitor::visitSizeOfExpr(SizeOf *expr) {
        if (expr->right()->kind() == TypeKind::FUNCTION) {
            _result.addError(
                    TypeCheckException(expr->right()->sourceRange(),
                            "Can not get sizeof a function type. Did you mean sizeof<ptr type>?"));
        }
        expr->type(std::make_shared<PrimitiveType>(expr->sourceRange(), PrimitiveTypeKind::U64));
    }

    void TypeCheckVisitor::visitCastExpr(Cast* expr) {
        check(expr->right());
        auto targetType = std::shared_ptr<Type>(expr->targetType()->clone());
        if (expr->targetType()->kind() == TypeKind::FUNCTION) {
            _result.addError(
                    TypeCheckException(expr->targetType()->sourceRange(),
                            "Can not cast to function type. Did you mean ptr to function?"));
        }
        expr->type(targetType);
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
        auto type = std::shared_ptr<Type>(expr->literalType()->clone());
        expr->type(type);
    }

    void TypeCheckVisitor::visitBoolLiteral(BoolLiteral* expr) {
        TypePtr type = std::make_shared<PrimitiveType>(expr->sourceRange(), PrimitiveTypeKind::BOOL);
        expr->type(type);
    }

    void TypeCheckVisitor::visitStringLiteral(StringLiteral* expr) {
        TypePtr type = std::make_shared<PointerType>(expr->sourceRange(),
                std::make_shared<PrimitiveType>(expr->sourceRange(), PrimitiveTypeKind::I8));
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
