#include "resolver.h"
#include "module.h"
#include "stmt.h"
#include "expr.h"
#include "type.h"
namespace klong {

    void Resolver::resolve(const StmtPtr& stmt) {
        if (stmt) {
            stmt->accept(this);
        }
    }

    void Resolver::resolve(const ExprPtr& expr) {
        if (expr) {
            expr->accept(this);
        }
    }

    // Module
    void Resolver::visitModule(Module* module) {
        // enterScope
        for (auto& statement : module->statements()) {
            resolve(statement);
        }
        // exitScope
    }

    // Stmt
    void Resolver::visitBlockStmt(Block* stmt) {
        // enterScope
        for (auto& statement : stmt->statements()) {
            resolve(statement);
        }
        // exitScope
    }

    void Resolver::visitExpressionStmt(Expression* stmt) {
        resolve(stmt->expression());
    }

    void Resolver::visitFunctionStmt(Function* stmt) {
        // declare function in outer name
        // go into new scope
        // declare params in inner scope
        for (auto& statement : stmt->body()) {
            resolve(statement);
        }
    }

    void Resolver::visitIfStmt(If* stmt) {
        resolve(stmt->condition());
        resolve(stmt->thenBranch());
        resolve(stmt->elseBranch());
    }

    void Resolver::visitPrintStmt(Print* stmt) {
        resolve(stmt->expression());
    }

    void Resolver::visitReturnStmt(Return* stmt) {
        resolve(stmt->value());
    }

    void Resolver::visitLetStmt(Let* stmt) {
        // declare stmt name
        if (stmt->initializer()) {
            resolve(stmt->initializer());
            // define stmt name
        }
    }

    void Resolver::visitConstStmt(Const* stmt) {
        // declare stmt name
        if (stmt->initializer()) {
            resolve(stmt->initializer());
            // define stmt name
        }
    }

    void Resolver::visitWhileStmt(While* stmt) {
        resolve(stmt->condition());
        resolve(stmt->body());
    }

    void Resolver::visitForStmt(For* stmt) {
        resolve(stmt->initializer());
        resolve(stmt->condition());
        resolve(stmt->increment());
        resolve(stmt->body());
    }

    void Resolver::visitCommentStmt(Comment* expr) {
        // empty on purpose
        (void) expr;
    }

    // Expr
    void Resolver::visitAssignExpr(Assign* expr) {
        resolve(expr->value());
        // TODO: resolveLocal ???
    }

    void Resolver::visitBinaryExpr(Binary* expr) {
        resolve(expr->left());
        resolve(expr->right());
    }

    void Resolver::visitCallExpr(Call* expr) {
        resolve(expr->callee());
        for (auto& arg : expr->args()) {
            resolve(arg);
        }
    }

    void Resolver::visitGroupingExpr(Grouping* expr) {
        resolve(expr->expression());
    }

    void Resolver::visitLogicalExpr(Logical* expr) {
        resolve(expr->left());
        resolve(expr->right());
    }

    void Resolver::visitUnaryExpr(Unary* expr) {
        resolve(expr->right());
    }

    void Resolver::visitVariableExpr(Variable* expr) {
        // TODO: implement variable resolving http://craftinginterpreters.com/resolving-and-binding.html
        (void) expr;
    }

    // Literals
    void Resolver::visitNumberLiteral(NumberLiteral* expr) {
        // nothing to do here
        (void) expr;
    }

    void Resolver::visitBoolLiteral(BoolLiteral* expr) {
        // nothing to do here
        (void) expr;
    }

    void Resolver::visitStringLiteral(StringLiteral* expr) {
        // nothing to do here
        (void) expr;
    }

    void Resolver::visitCharacterLiteral(CharacterLiteral* expr) {
        // nothing to do here
        (void) expr;
    }

    // Types
    void Resolver::visitFunctionType(FunctionType* type) {
        // nothing to do here
        (void) type;
    }

    void Resolver::visitPrimitiveType(PrimitiveType *type) {
        // nothing to do here
        (void) type;
    }

    void Resolver::visitSimpleType(SimpleType *type) {
        // nothing to do here
        (void) type;
    }
}