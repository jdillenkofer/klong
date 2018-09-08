#include "treewalk_terp.h"

#include <iostream>

namespace klong {

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
            for (const auto& stmt : statements) {
                execute(stmt.get());
            }
        } catch (const std::exception& exception) {
            this->_environment = previous;
            throw exception;
        }
        this->_environment = previous;
    }

    bool TreewalkTerp::isTruthy(std::any value) {
        return std::any_cast<bool>(value);
    }

    std::string TreewalkTerp::stringify(std::any value) {
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
        // TODO:
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
        std::cout << stringify(value) << std::flush;
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

    void TreewalkTerp::visitBinaryExpr(Binary* expr) {
        std::any left = evaluate(expr->left().get());
        std::any right = evaluate(expr->right().get());
        int64_t leftNumber = std::any_cast<int64_t>(left);
        int64_t rightNumber = std::any_cast<int64_t>(right);
        switch(expr->op()) {
            case BinaryOperation::EQUALITY:
                _valueOfLastExpr = leftNumber == rightNumber;
                return;
            case BinaryOperation::INEQUALITY:
                _valueOfLastExpr = leftNumber != rightNumber;
                return;
            case BinaryOperation::LESS_THAN:
                _valueOfLastExpr = leftNumber < rightNumber;
                return;
            case BinaryOperation::LESS_EQUAL:
                _valueOfLastExpr = leftNumber <= rightNumber;
                return;
            case BinaryOperation::GREATER_THAN:
                _valueOfLastExpr = leftNumber > rightNumber;
                return;
            case BinaryOperation::GREATER_EQUAL:
                _valueOfLastExpr = leftNumber >= rightNumber;
                return;
            case BinaryOperation::MINUS:
                _valueOfLastExpr = leftNumber - rightNumber;
                return;
            case BinaryOperation::PLUS:
                _valueOfLastExpr = leftNumber + rightNumber;
                return;
            case BinaryOperation::MULTIPLICATION:
                _valueOfLastExpr = leftNumber * rightNumber;
                return;
            case BinaryOperation::DIVISION:
                _valueOfLastExpr = leftNumber / rightNumber;
                return;
        }
    }

    void TreewalkTerp::visitCallExpr(Call* expr) {
        // TODO:
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
        return;
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