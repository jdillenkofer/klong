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

    bool TypeCheckVisitor::isBoolean(Expr* expr) {
        TypePtr type = expr->type();
        if (type->kind() == TypeKind::PRIMITIVE) {
            auto primitiveType = std::dynamic_pointer_cast<PrimitiveType>(type);
            return primitiveType->isBoolean();
        }
        return false;
    }

    bool TypeCheckVisitor::isFloat(Expr* expr) {
        TypePtr type = expr->type();
        if (type->kind() == TypeKind::PRIMITIVE) {
            auto primitiveType = std::dynamic_pointer_cast<PrimitiveType>(type);
            return primitiveType->isFloat();
        }
        return false;
    }

    bool TypeCheckVisitor::isInteger(Expr* expr) {
        TypePtr type = expr->type();
        if (type->kind() == TypeKind::PRIMITIVE) {
            auto primitiveType = std::dynamic_pointer_cast<PrimitiveType>(type);
            return primitiveType->isInteger();
        }
        return false;
    }

    bool TypeCheckVisitor::isString(Expr* expr) {
        TypePtr type = expr->type();
        if (type->kind() == TypeKind::PRIMITIVE) {
            auto primitiveType = std::dynamic_pointer_cast<PrimitiveType>(type);
            return primitiveType->isString();
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
        currentFunction = previousFunction;
    }

    void TypeCheckVisitor::visitParameterStmt(Parameter* stmt) {
        // nothing to do here
        (void) stmt;
    }

    void TypeCheckVisitor::visitIfStmt(If* stmt) {
        check(stmt->condition().get());
        if (!isBoolean(stmt->condition().get())) {
            throw TypeCheckException(stmt->condition()->sourceRange(), "Expect bool condition in if-statement.");
        }
        check(stmt->thenBranch().get());
        check(stmt->elseBranch().get());
    }

    void TypeCheckVisitor::visitPrintStmt(Print* stmt) {
        Expr* expr = stmt->expression().get();
        check(expr);
        if (!isString(expr) && !isInteger(expr) && !isFloat(expr) && !isBoolean(expr)) {
            throw TypeCheckException(stmt->sourceRange(), "Expect string, integer, float or boolean type in print stmt.");
        }
    }

    void TypeCheckVisitor::visitReturnStmt(Return* stmt) {
        check(stmt->value().get());
        if (stmt->value() != nullptr) {
            if (!currentFunction->functionType()->returnType()->isEqual(stmt->value()->type().get())) {
                throw TypeCheckException(stmt->sourceRange(), "Expect return statement type to match the function returnType.");
            }
        }
    }

    void TypeCheckVisitor::visitVarDeclStmt(VariableDeclaration* stmt) {
        if (currentFunction != nullptr && stmt->isPublic()) {
            throw TypeCheckException(stmt->sourceRange(), "Pub keyword not allowed in front of local variable.");
        }
        check(stmt->initializer().get());
        if (stmt->type() == nullptr) {
            stmt->type(std::shared_ptr<Type>(stmt->initializer()->type()->clone()));
        } else {
            if (stmt->initializer() && !stmt->type()->isEqual(stmt->initializer()->type().get())) {
                throw TypeCheckException(stmt->sourceRange(), "initializerType doesn't match declaration type.");
            }
        }
    }

    void TypeCheckVisitor::visitWhileStmt(While* stmt) {
        check(stmt->condition().get());
        if (!isBoolean(stmt->condition().get())) {
            throw TypeCheckException(stmt->condition()->sourceRange(), "while condition expects bool type.");
        }
        check(stmt->body().get());
    }

    void TypeCheckVisitor::visitForStmt(For* stmt) {
        check(stmt->initializer().get());
        check(stmt->condition().get());
        if (!isBoolean(stmt->condition().get())) {
            throw TypeCheckException(stmt->condition()->sourceRange(), "for condition expects bool type.");
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
        check(expr->value().get());
        if (!expr->target()->type()->isEqual(expr->value()->type().get())) {
            throw TypeCheckException(expr->value()->sourceRange(), "Expect valid type in assignment.");
        }
        expr->type(std::shared_ptr<Type>(expr->value()->type()->clone()));
    }

    void TypeCheckVisitor::visitBinaryExpr(Binary* expr) {
        check(expr->left().get());
        check(expr->right().get());
        // TODO: doubles and primitive type hierarchie
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
                    throw TypeCheckException(expr->sourceRange(), "Illegal operation for floating number.");
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
        throw TypeCheckException(expr->sourceRange(), "Illegal type in binary op.");
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
            auto calleePointer = std::dynamic_pointer_cast<PointerType>(calleeType);
            auto functionType = std::dynamic_pointer_cast<FunctionType>(calleePointer->pointsTo());
            if (functionType != nullptr) {
                if (!functionType->matchesSignature(callParamTypes)) {
                    throw TypeCheckException(expr->sourceRange(), "Call Expr doesn't match function signature.");
                }
                expr->type(std::shared_ptr<Type>(functionType->returnType()->clone()));
                return;
            }
        }
        throw TypeCheckException(expr->sourceRange(), "Callee doesn't resolve to function pointer expression.");
    }

    void TypeCheckVisitor::visitGroupingExpr(Grouping* expr) {
        check(expr->expression().get());
        expr->type(expr->expression()->type());
    }

    void TypeCheckVisitor::visitLogicalExpr(Logical* expr) {
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

    void TypeCheckVisitor::visitUnaryExpr(Unary* expr) {
        check(expr->right().get());
        if (expr->op() == UnaryOperation::NOT && !isBoolean(expr->right().get())) {
            throw TypeCheckException(expr->sourceRange(), "'!' expects boolean expression.");
        }
        if (expr->op() == UnaryOperation::MINUS && !isInteger(expr->right().get())) {
            throw TypeCheckException(expr->sourceRange(), "Unary '-' expects number expression.");
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
                throw TypeCheckException(expr->sourceRange(), "Variable resolves to invalid kind.");
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

    // Types
    void TypeCheckVisitor::visitFunctionType(FunctionType* type) {
        // nothing to do here
        (void) type;
    }

    void TypeCheckVisitor::visitPrimitiveType(PrimitiveType *type) {
        // nothing to do here
        (void) type;
    }

    void TypeCheckVisitor::visitPointerType(PointerType* type) {
        // nothing to do here
        (void) type;
    }

    void TypeCheckVisitor::visitSimpleType(SimpleType *type) {
        // nothing to do here
        (void) type;
    }
}
