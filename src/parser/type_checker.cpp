#include "type_checker.h"

#include "resolver.h"
#include "module.h"
#include "stmt.h"
#include "expr.h"
#include "type.h"

namespace klong {

    void TypeChecker::check(const std::vector<StmtPtr>& statements) {
        for (const auto& stmt : statements) {
            check(stmt);
        }
    }

    void TypeChecker::check(const StmtPtr& stmt) {
        if (stmt != nullptr) {
            stmt->accept(this);
        }
    }

    void TypeChecker::check(const ExprPtr& expr) {
        if (expr != nullptr) {
            expr->accept(this);
        }
    }

    bool TypeChecker::isBoolean(const ExprPtr& expr) {
        TypePtr type = expr->type();
        if (type->kind() == TypeKind::PRIMITIVE) {
            auto primitiveType = std::dynamic_pointer_cast<PrimitiveType>(type);
            if (primitiveType->type() == PrimitiveTypeKind::BOOL) {
                return true;
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
        check(stmt->expression());
    }

    void TypeChecker::visitFunctionStmt(Function* stmt) {
        // TODO: set functionVariable in this visitor, so that the returnStmt knows which function it is in
        check(stmt->body());
    }

    void TypeChecker::visitParameterStmt(Parameter* stmt) {
        // TODO:
        (void) stmt;
    }

    void TypeChecker::visitIfStmt(If* stmt) {
        check(stmt->condition());
        if (!isBoolean(stmt->condition())) {
            // TODO: throw
        }
        check(stmt->thenBranch());
        check(stmt->elseBranch());
    }

    void TypeChecker::visitPrintStmt(Print* stmt) {
        check(stmt->expression());
    }

    void TypeChecker::visitReturnStmt(Return* stmt) {
        // TODO we need a reference to the function this return stmt is in
        // to check if functionType == returnType
        check(stmt->value());
    }

    void TypeChecker::visitLetStmt(Let* stmt) {
        check(stmt->initializer());
        if (stmt->type() == nullptr) {
            // type propagation
        } else {
            if (stmt->initializer() && stmt->type() != stmt->initializer()->type()) {
                // TODO: throw
            }
        }
    }

    void TypeChecker::visitConstStmt(Const* stmt) {
        check(stmt->initializer());
        if (stmt->type() == nullptr) {
            // type propagation
        } else {
            if (stmt->type() != stmt->initializer()->type()) {
                // TODO: throw
            }
        }
    }

    void TypeChecker::visitWhileStmt(While* stmt) {
        check(stmt->condition());
        if (!isBoolean(stmt->condition())) {
            // TODO: throw
        }
        check(stmt->body());
    }

    void TypeChecker::visitForStmt(For* stmt) {
        check(stmt->initializer());
        check(stmt->condition());
        if (!isBoolean(stmt->condition())) {
            // TODO: throw
        }
        check(stmt->increment());
        check(stmt->body());
    }

    void TypeChecker::visitCommentStmt(Comment* expr) {
        // empty on purpose
        (void) expr;
    }

    // Expr
    void TypeChecker::visitAssignExpr(Assign* expr) {
        check(expr->target());
        check(expr->value());
        if (!expr->target()->type()->isEqual(expr->value()->type().get())) {
            // TODO: throw
        }
        // TODO: copy types without sourceRange
        expr->type(expr->value()->type());
    }

    void TypeChecker::visitBinaryExpr(Binary* expr) {
        check(expr->left());
        check(expr->right());
        // TODO: check all possible valid type combinations
    }

    void TypeChecker::visitCallExpr(Call* expr) {
        check(expr->callee());
        for (const auto& arg : expr->args()) {
            check(arg);
        }

        // TODO: create functionType and check with expr->resolvesTo
    }

    void TypeChecker::visitGroupingExpr(Grouping* expr) {
        check(expr->expression());
        expr->type(expr->expression()->type());
    }

    void TypeChecker::visitLogicalExpr(Logical* expr) {
        check(expr->left());
        check(expr->right());
        // TODO: check all possible valid type combinations
    }

    void TypeChecker::visitUnaryExpr(Unary* expr) {
        check(expr->right());
        // TODO: check all possible valid type combinations
    }

    void TypeChecker::visitVariableExpr(Variable* expr) {
        Stmt* resolvesTo = expr->resolvesTo();
        switch(resolvesTo->kind()) {
            case StatementKind::FUNCTION:
            {
                auto function = dynamic_cast<Function*>(resolvesTo);
                expr->type(function->functionType());
                break;
            }
            case StatementKind::CONST:
            {
                auto constStmt = dynamic_cast<Const*>(resolvesTo);
                expr->type(constStmt->type());
                break;
            }
            case StatementKind::LET:
            {
                auto letStmt = dynamic_cast<Let*>(resolvesTo);
                expr->type(letStmt->type());
                break;
            }
            default:
                // TODO: throw
                break;
        }
        // TODO: get type of resolvesTo and assign it to variable
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
