#include "resolve_visitor.h"
#include "ast/module.h"
#include "ast/stmt.h"
#include "ast/expr.h"
#include "ast/type.h"

namespace klong {

    void ResolveVisitor::resolve(Stmt* stmt) {
        if (stmt) {
            stmt->accept(this);
        }
    }

    void ResolveVisitor::resolve(Expr* expr) {
        if (expr) {
            expr->accept(this);
        }
    }

    void ResolveVisitor::resolve(const std::vector<Stmt*>& statements) {
        for (const auto& statement : statements) {
            if (statement->kind() == StatementKind::FUNCTION) {
                auto stmt = dynamic_cast<Function*>(statement);
                declare(stmt, stmt->name(), DeclarationType::FUNCTION);
                define(stmt->name());
            }
        }

        for (const auto& statement : statements) {
            resolve(statement);
        }
    }

    bool ResolveVisitor::resolveLocal(Variable* variable) {
        for (uint64_t i = _scopes.size(); i-- > 0;) {
            std::map<std::string, SymbolInfo> scope = _scopes[i];
            if (scope.find(variable->name()) != scope.end()) {
                SymbolInfo symbolInfo = scope.at(variable->name());
                variable->resolvesTo(symbolInfo.declarationStmt);
                return true;
            }
        }
        _result->addError(
                CompilationError(variable->sourceRange(), "Couldn't resolve variable '" + variable->name() + "'."));
        return false;
    }

    void ResolveVisitor::enterScope() {
        _scopes.emplace_back(std::map<std::string, SymbolInfo>());
    }

    void ResolveVisitor::exitScope() {
        _scopes.pop_back();
    }

    void ResolveVisitor::declare(Stmt* declarationStmt, std::string name, DeclarationType declarationType) {
        if (_scopes.empty()) {
            return;
        }
        std::map<std::string, SymbolInfo>& scope = _scopes.back();
        if (scope.find(name) != scope.end()) {
            _result->addError(CompilationError(declarationStmt->sourceRange(),
                    "Symbol with name '" + name + "' already declared in this scope"));
        }
        scope.insert(std::pair<std::string, SymbolInfo>(name, SymbolInfo { declarationStmt, declarationType, false }));
    }

    void ResolveVisitor::define(std::string name) {
        if (_scopes.empty()) {
            return;
        }
        std::map<std::string, SymbolInfo>& scope = _scopes.back();
        auto& symbol = scope.at(name);
        symbol.initialized = true;
    }

    // Module
    void ResolveVisitor::visitModule(Module* module) {
        for (auto& stmt : module->statements()) {
            if (stmt->kind() != StatementKind::FUNCTION 
				&& stmt->kind() != StatementKind::VAR_DECL
				&& stmt->kind() != StatementKind::TYPE_DECL
                && stmt->kind() != StatementKind::EXT_DECL
                && stmt->kind() != StatementKind::COMMENT) {
                _result->addError(
                        CompilationError(stmt->sourceRange(), "Illegal top level statement."));
            }
        }
        enterScope();
        resolve(module->statements());
        exitScope();
    }

    // Stmt
    void ResolveVisitor::visitBlockStmt(Block* stmt) {
        enterScope();
        resolve(stmt->statements());
        exitScope();
    }

    void ResolveVisitor::visitExpressionStmt(Expression* stmt) {
        resolve(stmt->expression());
    }

    void ResolveVisitor::visitExtDeclStmt(ExternalDeclaration* stmt) {
        auto type = stmt->type();
        auto declType = type->kind() == TypeKind::FUNCTION ? DeclarationType::FUNCTION : DeclarationType::CONST;
        declare(stmt, stmt->name(), declType);
        define(stmt->name());
    }

    void ResolveVisitor::visitFunctionStmt(Function* stmt) {
        resolveFunction(stmt, true);
    }

    void ResolveVisitor::visitParameterStmt(Parameter* stmt) {
        declare(stmt, stmt->name(), DeclarationType::PARAM);
        define(stmt->name());
    }

    void ResolveVisitor::resolveFunction(Function *stmt, bool insideFunction) {
        bool enclosingFunction = _isInsideFunction;
        _isInsideFunction = insideFunction;
        enterScope();
        for (const auto& param : stmt->params()) {
            resolve(param);
        }

        resolve(stmt->body());
        exitScope();
        _isInsideFunction = enclosingFunction;
    }

    void ResolveVisitor::visitIfStmt(If* stmt) {
        resolve(stmt->condition());
        resolve(stmt->thenBranch());
        resolve(stmt->elseBranch());
    }

    void ResolveVisitor::visitReturnStmt(Return* stmt) {
        if (!_isInsideFunction) {
            _result->addError(
                    CompilationError(stmt->sourceRange(), "Cannot return from top-level code."));
        }
        resolve(stmt->value());
    }

    void ResolveVisitor::visitVarDeclStmt(VariableDeclaration* stmt) {
        if (stmt->initializer()) {
            resolve(stmt->initializer());
        }
        auto declType = stmt->isConst() ? DeclarationType::CONST : DeclarationType::LET;
        declare(stmt, stmt->name(), declType);
        if (stmt->initializer()) {
            define(stmt->name());
        }
        if (stmt->type()
        && (stmt->type()->kind() == TypeKind::CUSTOM
        || (stmt->type()->kind() == TypeKind::POINTER
            && dynamic_cast<PointerType*>(stmt->type())->isArray()))) {
            // TODO: should custom types always be auto resolved?
            define(stmt->name());
        }
    }

	void ResolveVisitor::visitStructDeclStmt(StructDeclaration* stmt) {
        // nothing to do here
        (void) stmt;
	}

    void ResolveVisitor::visitUnionDeclStmt(UnionDeclaration* stmt) {
        // nothing to do here
        (void) stmt;
    }

	void ResolveVisitor::visitEnumDeclStmt(EnumDeclaration* stmt) {
		// nothing to do here
		(void) stmt;
	}

	void ResolveVisitor::visitCustomMemberStmt(CustomMember* stmt) {
        // nothing to do here
        (void) stmt;
	}

    void ResolveVisitor::visitWhileStmt(While* stmt) {
        resolve(stmt->condition());
        resolve(stmt->body());
    }

    void ResolveVisitor::visitForStmt(For* stmt) {
        enterScope();
        resolve(stmt->initializer());
        resolve(stmt->condition());
        resolve(stmt->increment());
        resolve(stmt->body());
        exitScope();
    }

    void ResolveVisitor::visitBreakStmt(Break* stmt) {
		// nothing to do here
        (void) stmt;
    }

    void ResolveVisitor::visitContinueStmt(Continue* stmt) {
		// nothing to do here
        (void) stmt;
    }

    void ResolveVisitor::visitDeferStmt(Defer* stmt) {
        resolve(stmt->stmtToDefer());
    }

    void ResolveVisitor::visitCommentStmt(Comment* expr) {
		// nothing to do here
        (void) expr;
    }

    // Expr
    void ResolveVisitor::visitAssignExpr(Assign* expr) {
        resolve(expr->value());
        if (expr->isTargetVariable()) {
            auto isResolved = resolveLocal(expr->target());
            auto varDeclRes = expr->target()->resolvesTo();
            if (isResolved && varDeclRes->kind() == StatementKind::VAR_DECL) {
                auto varDecl = dynamic_cast<VariableDeclaration*>(varDeclRes);
                if (varDecl->isConst()) {
                    _result->addError(
                            CompilationError(expr->sourceRange(), "Cannot reassign 'const'."));
                }
            }
        } else {
            resolve(expr->targetExpr());
        }
    }

    void ResolveVisitor::visitBinaryExpr(Binary* expr) {
        resolve(expr->left());
        resolve(expr->right());
    }

    void ResolveVisitor::visitCallExpr(Call* expr) {
        resolve(expr->callee());
        for (const auto& arg : expr->args()) {
            resolve(arg);
        }
    }

    void ResolveVisitor::visitGroupingExpr(Grouping* expr) {
        resolve(expr->expression());
    }

	void ResolveVisitor::visitSubscriptExpr(Subscript* expr) {
		resolve(expr->target());
		resolve(expr->index());
	}

	void ResolveVisitor::visitMemberAccessExpr(MemberAccess* expr) {
        resolve(expr->target());
    }

	void ResolveVisitor::visitEnumAccessExpr(EnumAccess* expr) {
		// nothing to do here
		(void) expr;
	}

    void ResolveVisitor::visitLogicalExpr(Logical* expr) {
        resolve(expr->left());
        resolve(expr->right());
    }

    void ResolveVisitor::visitUnaryExpr(Unary* expr) {
        resolve(expr->right());
    }

    void ResolveVisitor::visitSizeOfExpr(SizeOf* expr) {
		// nothing to do here
        (void) expr;
    }

    void ResolveVisitor::visitCastExpr(Cast* expr) {
        resolve(expr->right());
    }

    void ResolveVisitor::visitVariableExpr(Variable* expr) {
        std::map<std::string, SymbolInfo>& scope = _scopes.back();
        if (scope.find(expr->name()) != scope.end()) {
            auto& symbol = (*scope.find(expr->name())).second;
            if (!symbol.initialized) {
                _result->addError(
                        CompilationError(expr->sourceRange(), "Cannot read local variable in its own initializer."));
            }
        }
        resolveLocal(expr);
    }

    // Literals
    void ResolveVisitor::visitNumberLiteral(NumberLiteral* expr) {
        // nothing to do here
        (void) expr;
    }

    void ResolveVisitor::visitBoolLiteral(BoolLiteral* expr) {
        // nothing to do here
        (void) expr;
    }

    void ResolveVisitor::visitStringLiteral(StringLiteral* expr) {
        // nothing to do here
        (void) expr;
    }

    void ResolveVisitor::visitCharacterLiteral(CharacterLiteral* expr) {
        // nothing to do here
        (void) expr;
    }

    void ResolveVisitor::visitArrayLiteral(ArrayLiteral* expr) {
        // nothing to do here
        (void) expr;
    }
}