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

    void Resolver::resolveLocal(Variable* variable) {
        for (int64_t i = _scopes.size() - 1; i >= 0; i--) {
            std::map<std::string, SymbolInfo> scope = _scopes[i];
            if (scope.find(variable->name()) != scope.end()) {
                SymbolInfo symbolInfo = scope.at(variable->name());
                variable->resolvesTo(symbolInfo.declarationStmt);
                return;
            }
        }
        throw ResolveException(variable->sourceRange(), "Couldn't resolve variable '" + variable->name() + "'.");
    }

    void Resolver::enterScope() {
        _scopes.emplace_back(std::map<std::string, SymbolInfo>());
    }

    void Resolver::exitScope() {
        _scopes.pop_back();
    }

    void Resolver::declare(Stmt* declarationStmt, std::string name, DeclarationType declarationType) {
        if (_scopes.empty()) {
            return;
        }
        std::map<std::string, SymbolInfo>& scope = _scopes.back();
        if (scope.find(name) != scope.end()) {
            throw ResolveException(declarationStmt->sourceRange(),
                    "Variable with name '" + name + "' already declared in this scope");
        }
        scope.insert(std::pair<std::string, SymbolInfo>(name, SymbolInfo { declarationStmt, declarationType, false }));
    }

    void Resolver::define(std::string name) {
        if (_scopes.empty()) {
            return;
        }
        std::map<std::string, SymbolInfo>& scope = _scopes.back();
        auto& symbol = scope.at(name);
        symbol.initialized = true;
    }

    // Module
    void Resolver::visitModule(Module* module) {
        enterScope();
        for (auto& statement : module->statements()) {
            resolve(statement);
        }
        exitScope();
    }

    // Stmt
    void Resolver::visitBlockStmt(Block* stmt) {
        enterScope();
        for (auto& statement : stmt->statements()) {
            resolve(statement);
        }
        exitScope();
    }

    void Resolver::visitExpressionStmt(Expression* stmt) {
        resolve(stmt->expression());
    }

    void Resolver::visitFunctionStmt(Function* stmt) {
        declare(stmt, stmt->name(), DeclarationType::FUNCTION_DECL);
        define(stmt->name());

        resolveFunction(stmt, true);
    }

    void Resolver::visitParameterStmt(Parameter* stmt) {
        declare(stmt, stmt->name(), DeclarationType::PARAM_DECL);
        define(stmt->name());
    }

    void Resolver::resolveFunction(klong::Function *stmt, bool insideFunction) {
        bool enclosingFunction = _isInsideFunction;
        _isInsideFunction = insideFunction;
        enterScope();
        for (const auto& param : stmt->params()) {
            resolve(param);
        }

        for (const auto& statement : stmt->body()) {
            resolve(statement);
        }
        exitScope();
        _isInsideFunction = enclosingFunction;
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
        if (!_isInsideFunction) {
            throw ResolveException(stmt->sourceRange(), "Cannot return from top-level code.");
        }
        resolve(stmt->value());
    }

    void Resolver::visitLetStmt(Let* stmt) {
        declare(stmt, stmt->name(), DeclarationType::LET);
        if (stmt->initializer()) {
            resolve(stmt->initializer());
            define(stmt->name());
        }
    }

    void Resolver::visitConstStmt(Const* stmt) {
        declare(stmt, stmt->name(), DeclarationType::CONST);
        if (stmt->initializer()) {
            resolve(stmt->initializer());
            define(stmt->name());
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
        resolveLocal(expr->target().get());
        if (expr->target()->resolvesTo()->kind() == StatementKind::CONST) {
            throw ResolveException(expr->sourceRange(), "Cannot reassign 'const'.");
        }
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
        std::map<std::string, SymbolInfo>& scope = _scopes.back();
        if (scope.find(expr->name()) != scope.end()) {
            auto& symbol = (*scope.find(expr->name())).second;
            if (!symbol.initialized) {
                throw ResolveException(expr->sourceRange(), "Cannot read local variable in its own initializer.");
            }
        }
        resolveLocal(expr);
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