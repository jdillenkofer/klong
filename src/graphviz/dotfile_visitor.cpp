#include "dotfile_visitor.h"

#include "ast/module.h"
#include "ast/stmt.h"
#include "ast/expr.h"
#include "ast/type.h"

namespace klong {
    // Module
    void DotfileVisitor::visitModule(Module* module) {
        // start digraph
        appendLine("digraph \"" + module->filename() + "\"  {");

        // options
        appendLine("node [shape=record fontname=Arial];");

        // print module stuff here
        auto moduleId = getModuleId(module);
        appendLine(std::to_string(moduleId) + " [label=\"Module\\n" + module->filename() + "\\n\"]");

        // visit statements
        for (auto& statement : module->statements()) {
            statement->accept(this);
            auto stmtId = getStmtId(statement.get());
            appendLine(std::to_string(moduleId) + " -> " + std::to_string(stmtId));
        }
        appendLine("}");
    }

    // Stmt
    void DotfileVisitor::visitBlockStmt(Block* stmt) {
        // print block stuff here
        auto blockId = getStmtId(stmt);
        appendLine(std::to_string(blockId) + " [label=\"Block\"]");

        // visit statements
        for (auto& statement : stmt->statements()) {
            statement->accept(this);
            auto stmtId = getStmtId(statement.get());
            appendLine(std::to_string(blockId) + " -> " + std::to_string(stmtId));
        }
    }

    void DotfileVisitor::visitExpressionStmt(Expression* stmt) {
        // print ExpressionStmt stuff here
        auto expressionStmtId = getStmtId(stmt);
        appendLine(std::to_string(expressionStmtId) + " [label=\"ExpressionStmt\"]");

        // visit expression
        stmt->expression()->accept(this);
        auto expressionId = getExprId(stmt->expression().get());
        appendLine(std::to_string(expressionStmtId) + " -> " + std::to_string(expressionId));
    }

    void DotfileVisitor::visitExtDeclStmt(ExternalDeclaration* stmt) {
        // print ExtDeclStmt stuff here
        auto externalDeclStmtId = getStmtId(stmt);
        appendLine(std::to_string(externalDeclStmtId) + " [label=\"ExternalDeclaration\\n" + stmt->name() + "\"]");
    }

    void DotfileVisitor::visitFunctionStmt(Function* stmt) {
        // print FunctionStmt stuff here
        auto functionStmtId = getStmtId(stmt);
        appendLine(std::to_string(functionStmtId) + " [label=\"Function\\n" + stmt->name() + "\"]");

        for (auto& param : stmt->params()) {
            param->accept(this);
            auto paramStmtId = getStmtId(param.get());
            appendLine(std::to_string(functionStmtId) + " -> " + std::to_string(paramStmtId));
        }

        // visit FunctionBody
        for (auto& statement : stmt->body()) {
            statement->accept(this);
            auto stmtId = getStmtId(statement.get());
            appendLine(std::to_string(functionStmtId) + " -> " + std::to_string(stmtId));
        }
    }

    void DotfileVisitor::visitParameterStmt(Parameter* stmt) {
        // print ParameterStmt stuff here
        auto parameterStmtId = getStmtId(stmt);
        appendLine(std::to_string(parameterStmtId) + " [label=\"Parameter\\n" + stmt->name() + "\"]");
    }

    void DotfileVisitor::visitIfStmt(If* stmt) {
        // print IfStmt stuff here
        auto ifStmtId = getStmtId(stmt);
        appendLine(std::to_string(ifStmtId) + " [label=\"If\"]");

        // visit condition
        stmt->condition()->accept(this);
        auto conditionExprId = getExprId(stmt->condition().get());
        appendLine(std::to_string(ifStmtId) + " -> " + std::to_string(conditionExprId));

        // visit then
        stmt->thenBranch()->accept(this);
        auto thenBranchStmtId = getStmtId(stmt->thenBranch().get());
        appendLine(std::to_string(ifStmtId) + " -> " + std::to_string(thenBranchStmtId));

        // visit else
        if (stmt->elseBranch() != nullptr) {
            stmt->elseBranch()->accept(this);
            auto elseBranchStmtId = getStmtId(stmt->elseBranch().get());
            appendLine(std::to_string(ifStmtId) + " -> " + std::to_string(elseBranchStmtId));
        }
    }

    void DotfileVisitor::visitPrintStmt(Print* stmt) {
        // print PrintStmt stuff here
        auto printStmtId = getStmtId(stmt);
        appendLine(std::to_string(printStmtId) + " [label=\"Print\"]");

        // visit expression
        stmt->expression()->accept(this);
        auto exprId = getExprId(stmt->expression().get());
        appendLine(std::to_string(printStmtId) + " -> " + std::to_string(exprId));
    }

    void DotfileVisitor::visitReturnStmt(Return* stmt) {
        // print ReturnStmt stuff here
        auto returnStmtId = getStmtId(stmt);
        appendLine(std::to_string(returnStmtId) + " [label=\"Return\"]");

        // visit value expression
        stmt->value()->accept(this);
        auto valueExprId = getExprId(stmt->value().get());
        appendLine(std::to_string(returnStmtId) + " -> " + std::to_string(valueExprId));
    }

    void DotfileVisitor::visitVarDeclStmt(VariableDeclaration* stmt) {
        // print VariableDeclaration stuff here
        auto varDeclStmtId = getStmtId(stmt);
        appendLine(std::to_string(varDeclStmtId) + " [label=\"VariableDeclaration\\n" + stmt->name() + "\\n"
        + "isConst: " + std::to_string(stmt->isConst()) + "\\n"
        + "isPublic: " + std::to_string(stmt->isPublic()) + "\"]");

        // visit initializer
        stmt->initializer()->accept(this);
        auto initializerExprId = getExprId(stmt->initializer().get());
        appendLine(std::to_string(varDeclStmtId) + " -> " + std::to_string(initializerExprId));
    }

    void DotfileVisitor::visitWhileStmt(While* stmt) {
        // print While stuff here
        auto whileStmtId = getStmtId(stmt);
        appendLine(std::to_string(whileStmtId) + " [label=\"While\"]");

        // visit condition
        stmt->condition()->accept(this);
        auto conditionExprId = getExprId(stmt->condition().get());
        appendLine(std::to_string(whileStmtId) + " -> " + std::to_string(conditionExprId));
        // visit body
        stmt->body()->accept(this);
        auto bodyStmtId = getStmtId(stmt->body().get());
        appendLine(std::to_string(whileStmtId) + " -> " + std::to_string(bodyStmtId));
    }

    void DotfileVisitor::visitForStmt(For* stmt) {
        // print For stuff here
        auto forStmtId = getStmtId(stmt);
        appendLine(std::to_string(forStmtId) + " [label=\"For\"]");

        // visit initializer
        if (stmt->initializer() != nullptr) {
            stmt->initializer()->accept(this);
            auto initializerStmtId = getStmtId(stmt->initializer().get());
            appendLine(std::to_string(forStmtId) + " -> " + std::to_string(initializerStmtId));
        }
        // visit condition
        stmt->condition()->accept(this);
        auto conditionExprId = getExprId(stmt->condition().get());
        appendLine(std::to_string(forStmtId) + " -> " + std::to_string(conditionExprId));

        // visit increment
        if (stmt->increment() != nullptr) {
            stmt->increment()->accept(this);
            auto incrementExprId = getExprId(stmt->increment().get());
            appendLine(std::to_string(forStmtId) + " -> " + std::to_string(incrementExprId));
        }

        // visit body
        stmt->body()->accept(this);
        auto bodyStmtId = getStmtId(stmt->body().get());
        appendLine(std::to_string(forStmtId) + " -> " + std::to_string(bodyStmtId));
    }

    void DotfileVisitor::visitCommentStmt(Comment* stmt) {
        // print Comment stuff here
        auto commentStmtId = getStmtId(stmt);
        appendLine(std::to_string(commentStmtId) + " [label=\"Comment\"]");
    }

    // Expr
    void DotfileVisitor::visitAssignExpr(Assign* expr) {
        // print Assignment stuff here
        auto assignmentExprId = getExprId(expr);
        appendLine(std::to_string(assignmentExprId) + " [label=\"Assignment\"]");

        // visit target
        expr->target()->accept(this);
        auto targetExprId = getExprId(expr->target().get());
        appendLine(std::to_string(assignmentExprId) + " -> " + std::to_string(targetExprId));

        // visit value
        expr->value()->accept(this);
        auto valueExprId = getExprId(expr->value().get());
        appendLine(std::to_string(assignmentExprId) + " -> " + std::to_string(valueExprId));
    }

    void DotfileVisitor::visitBinaryExpr(Binary* expr) {
        // print Binary stuff here
        auto binaryExprId = getExprId(expr);
        appendLine(std::to_string(binaryExprId) + " [label=\"Binary\"]");

        // visit left
        expr->left()->accept(this);
        auto leftExprId = getExprId(expr->left().get());
        appendLine(std::to_string(binaryExprId) + " -> " + std::to_string(leftExprId));

        // visit right
        expr->right()->accept(this);
        auto rightExprId = getExprId(expr->right().get());
        appendLine(std::to_string(binaryExprId) + " -> " + std::to_string(rightExprId));
    }

    void DotfileVisitor::visitCallExpr(Call* expr) {
        // print Call stuff here
        auto callExprId = getExprId(expr);
        appendLine(std::to_string(callExprId) + " [label=\"Call\"]");

        // visit callee
        expr->callee()->accept(this);
        auto calleeId = getExprId(expr->callee().get());
        appendLine(std::to_string(callExprId) + " -> " + std::to_string(calleeId));
    }

    void DotfileVisitor::visitGroupingExpr(Grouping* expr) {
        // print Grouping stuff here
        auto groupingExprId = getExprId(expr);
        appendLine(std::to_string(groupingExprId) + " [label=\"Grouping\"]");

        // visit expression
        expr->expression()->accept(this);
        auto exprId = getExprId(expr->expression().get());
        appendLine(std::to_string(groupingExprId) + " -> " + std::to_string(exprId));
    }

    void DotfileVisitor::visitLogicalExpr(Logical* expr) {
        // print Logical stuff here
        auto logicalExprId = getExprId(expr);
        appendLine(std::to_string(logicalExprId) + " [label=\"Logical\"]");

        // visit left
        expr->left()->accept(this);
        auto leftExprId = getExprId(expr->left().get());
        appendLine(std::to_string(logicalExprId) + " -> " + std::to_string(leftExprId));

        // visit right
        expr->right()->accept(this);
        auto rightExprId = getExprId(expr->right().get());
        appendLine(std::to_string(logicalExprId) + " -> " + std::to_string(rightExprId));
    }

    void DotfileVisitor::visitUnaryExpr(Unary* expr) {
        // print Unary stuff here
        auto unaryExprId = getExprId(expr);
        appendLine(std::to_string(unaryExprId) + " [label=\"Unary\"]");

        // visit right
        expr->right()->accept(this);
        auto rightExprId = getExprId(expr->right().get());
        appendLine(std::to_string(unaryExprId) + " -> " + std::to_string(rightExprId));
    }

    void DotfileVisitor::visitVariableExpr(Variable* expr) {
        // print Variable stuff here
        auto variableExprId = getExprId(expr);
        appendLine(std::to_string(variableExprId) + " [label=\"Variable\"]");

        // visit resolvesTo
        auto resolvesToId = getStmtId(expr->resolvesTo());
        appendLine(std::to_string(variableExprId) + " -> " + std::to_string(resolvesToId));
    }

    // Literals
    void DotfileVisitor::visitNumberLiteral(NumberLiteral* expr) {
        // print NumberLiteral stuff here
        auto numberLiteralId = getExprId(expr);
        appendLine(std::to_string(numberLiteralId) + " [label=\"NumberLiteral\\n"
        + "i64: " + std::to_string(expr->i64()) + "\\n"
        + "u64: " + std::to_string(expr->u64()) + "\\n"
        + "f64: " + std::to_string(expr->f64()) + "\"]");
    }

    void DotfileVisitor::visitBoolLiteral(BoolLiteral* expr) {
        // print BoolLiteral stuff here
        auto boolLiteralId = getExprId(expr);
        appendLine(std::to_string(boolLiteralId) + " [label=\"BoolLiteral\\n"
                   + "i64: " + std::to_string(expr->value()) + "\"]");
    }

    void DotfileVisitor::visitStringLiteral(StringLiteral* expr) {
        // print StringLiteral stuff here
        auto stringLiteralId = getExprId(expr);
        appendLine(std::to_string(stringLiteralId) + " [label=\"StringLiteral\\n"
                   + "value: " + expr->value() + "\"]");
    }

    void DotfileVisitor::visitCharacterLiteral(CharacterLiteral* expr) {
        // print CharacterLiteral stuff here
        auto boolLiteralId = getExprId(expr);
        appendLine(std::to_string(boolLiteralId) + " [label=\"CharLiteral\\n"
                   + "value: '" + std::to_string(expr->value()) + "'\"]");
    }

    // Types
    void DotfileVisitor::visitFunctionType(FunctionType* type) {

    }

    void DotfileVisitor::visitPrimitiveType(PrimitiveType *type) {

    }

    void DotfileVisitor::visitPointerType(PointerType *type) {

    }

    void DotfileVisitor::visitSimpleType(SimpleType *type) {

    }

    void DotfileVisitor::resetIdCounter() {
        _counter = 0;
        _moduleToId.clear();
        _stmtToId.clear();
        _exprToId.clear();
        _typeToId.clear();
        _output = "";
    }

    uint64_t DotfileVisitor::getModuleId(Module* module) {
        if (_moduleToId.find(module) != _moduleToId.end()) {
            return _moduleToId[module];
        }
        auto id = _counter++;
        _moduleToId[module] = id;
        return id;
    }

    uint64_t DotfileVisitor::getStmtId(Stmt* stmt) {
        if (_stmtToId.find(stmt) != _stmtToId.end()) {
            return _stmtToId[stmt];
        }
        auto id = _counter++;
        _stmtToId[stmt] = id;
        return id;
    }

    uint64_t DotfileVisitor::getExprId(Expr* expr) {
        if (_exprToId.find(expr) != _exprToId.end()) {
            return _exprToId[expr];
        }
        auto id = _counter++;
        _exprToId[expr] = id;
        return id;
    }

    uint64_t DotfileVisitor::getTypeId(Type* type) {
        if (_typeToId.find(type) != _typeToId.end()) {
            return _typeToId[type];
        }
        auto id = _counter++;
        _typeToId[type] = id;
        return id;
    }
}