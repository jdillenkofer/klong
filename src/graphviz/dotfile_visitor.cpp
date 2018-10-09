#include "dotfile_visitor.h"

#include "ast/module.h"
#include "ast/stmt.h"
#include "ast/expr.h"
#include "ast/type.h"
#include "ast/string_helper.h"

namespace klong {
    // Module
    void DotfileVisitor::visitModule(Module* module) {
        // start digraph
        appendLine("digraph \"" + module->filename() + "\"  {");

        // options
        appendLine("node [shape=record fontname=Arial];");
        appendLine("edge [fontname=Arial];");

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
        appendLine(std::to_string(externalDeclStmtId) + " [label=\"ExternalDeclaration\\n" + stmt->name() + "\\n"
        + getType(stmt->type().get())+ "\"]");
    }

    void DotfileVisitor::visitFunctionStmt(Function* stmt) {
        // print FunctionStmt stuff here
        auto functionStmtId = getStmtId(stmt);
        appendLine(std::to_string(functionStmtId) + " [label=\"Function\\n" + stmt->name() + "\\n"
        + getType(stmt->functionType().get())+ "\"]");

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
        appendLine(std::to_string(parameterStmtId) + " [label=\"Parameter\\n" + stmt->name() + "\\n"
        + getType(stmt->type().get())+ "\"]");
    }

    void DotfileVisitor::visitIfStmt(If* stmt) {
        // print IfStmt stuff here
        auto ifStmtId = getStmtId(stmt);
        appendLine(std::to_string(ifStmtId) + " [label=\"If\"]");

        // visit condition
        stmt->condition()->accept(this);
        auto conditionExprId = getExprId(stmt->condition().get());
        appendLine(std::to_string(ifStmtId) + " -> " + std::to_string(conditionExprId)+ " [ label=\"condition\" ]");

        // visit then
        stmt->thenBranch()->accept(this);
        auto thenBranchStmtId = getStmtId(stmt->thenBranch().get());
        appendLine(std::to_string(ifStmtId) + " -> " + std::to_string(thenBranchStmtId)+ " [ label=\"then\" ]");

        // visit else
        if (stmt->elseBranch() != nullptr) {
            stmt->elseBranch()->accept(this);
            auto elseBranchStmtId = getStmtId(stmt->elseBranch().get());
            appendLine(std::to_string(ifStmtId) + " -> " + std::to_string(elseBranchStmtId)+ " [ label=\"else\" ]");
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
        + "isPublic: " + to_string(stmt->isPublic()) + "\\n"
        + "isConst: " + to_string(stmt->isConst()) + "\\n"
        + getType(stmt->type().get())+ "\"]");

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
        appendLine(std::to_string(whileStmtId) + " -> " + std::to_string(conditionExprId) + " [ label=\"condition\" ]");
        // visit body
        stmt->body()->accept(this);
        auto bodyStmtId = getStmtId(stmt->body().get());
        appendLine(std::to_string(whileStmtId) + " -> " + std::to_string(bodyStmtId)+ " [ label=\"body\" ]");
    }

    void DotfileVisitor::visitForStmt(For* stmt) {
        // print For stuff here
        auto forStmtId = getStmtId(stmt);
        appendLine(std::to_string(forStmtId) + " [label=\"For\"]");

        // visit initializer
        if (stmt->initializer() != nullptr) {
            stmt->initializer()->accept(this);
            auto initializerStmtId = getStmtId(stmt->initializer().get());
            appendLine(std::to_string(forStmtId) + " -> " + std::to_string(initializerStmtId) + " [ label=\"initializer\" ]");
        }
        // visit condition
        stmt->condition()->accept(this);
        auto conditionExprId = getExprId(stmt->condition().get());
        appendLine(std::to_string(forStmtId) + " -> " + std::to_string(conditionExprId) + " [ label=\"condition\" ]");

        // visit increment
        if (stmt->increment() != nullptr) {
            stmt->increment()->accept(this);
            auto incrementExprId = getExprId(stmt->increment().get());
            appendLine(std::to_string(forStmtId) + " -> " + std::to_string(incrementExprId) + " [ label=\"increment\" ]");
        }

        // visit body
        stmt->body()->accept(this);
        auto bodyStmtId = getStmtId(stmt->body().get());
        appendLine(std::to_string(forStmtId) + " -> " + std::to_string(bodyStmtId)+ " [ label=\"body\" ]");
    }

    void DotfileVisitor::visitCommentStmt(Comment* stmt) {
        // print Comment stuff here
        auto commentStmtId = getStmtId(stmt);
        appendLine(std::to_string(commentStmtId) + " [label=\"Comment\\n" + stmt->text() + "\"]");
    }

    // Expr
    void DotfileVisitor::visitAssignExpr(Assign* expr) {
        // print Assignment stuff here
        auto assignmentExprId = getExprId(expr);
        appendLine(std::to_string(assignmentExprId) + " [label=\"Assignment\\n"+ getType(expr->type().get()) + "\"]");

        // visit target
        expr->target()->accept(this);
        auto targetExprId = getExprId(expr->target().get());
        appendLine(std::to_string(assignmentExprId) + " -> " + std::to_string(targetExprId)+ " [ label=\"target\" ]");

        // visit value
        expr->value()->accept(this);
        auto valueExprId = getExprId(expr->value().get());
        appendLine(std::to_string(assignmentExprId) + " -> " + std::to_string(valueExprId)+ " [ label=\"value\" ]");
    }

    void DotfileVisitor::visitBinaryExpr(Binary* expr) {
        // print Binary stuff here
        auto binaryExprId = getExprId(expr);
        appendLine(std::to_string(binaryExprId) + " [label=\"Binary\\n"
        + to_string(expr->op()) + "\\n" +
        getType(expr->type().get())+ "\"]");

        // visit left
        expr->left()->accept(this);
        auto leftExprId = getExprId(expr->left().get());
        appendLine(std::to_string(binaryExprId) + " -> " + std::to_string(leftExprId)+ " [ label=\"left\" ]");

        // visit right
        expr->right()->accept(this);
        auto rightExprId = getExprId(expr->right().get());
        appendLine(std::to_string(binaryExprId) + " -> " + std::to_string(rightExprId)+ " [ label=\"right\" ]");
    }

    void DotfileVisitor::visitCallExpr(Call* expr) {
        // print Call stuff here
        auto callExprId = getExprId(expr);
        appendLine(std::to_string(callExprId) + " [label=\"Call\\n" + getType(expr->type().get()) + "\"]");

        // visit callee
        expr->callee()->accept(this);
        auto calleeId = getExprId(expr->callee().get());
        appendLine(std::to_string(callExprId) + " -> " + std::to_string(calleeId) + "[ label=\"callee\" ]");

        // visit args
        for (auto& arg : expr->args()) {
            arg->accept(this);
            auto argId = getExprId(arg.get());
            appendLine(std::to_string(callExprId) + " -> " + std::to_string(argId));
        }
    }

    void DotfileVisitor::visitGroupingExpr(Grouping* expr) {
        // print Grouping stuff here
        auto groupingExprId = getExprId(expr);
        appendLine(std::to_string(groupingExprId) + " [label=\"Grouping\\n" + getType(expr->type().get()) + "\"]");

        // visit expression
        expr->expression()->accept(this);
        auto exprId = getExprId(expr->expression().get());
        appendLine(std::to_string(groupingExprId) + " -> " + std::to_string(exprId));
    }

    void DotfileVisitor::visitLogicalExpr(Logical* expr) {
        // print Logical stuff here
        auto logicalExprId = getExprId(expr);
        appendLine(std::to_string(logicalExprId) + " [label=\"Logical\\n"
        + to_string(expr->op()) + "\\n"
        + getType(expr->type().get())+ "\"]");

        // visit left
        expr->left()->accept(this);
        auto leftExprId = getExprId(expr->left().get());
        appendLine(std::to_string(logicalExprId) + " -> " + std::to_string(leftExprId)+ " [ label=\"left\" ]");

        // visit right
        expr->right()->accept(this);
        auto rightExprId = getExprId(expr->right().get());
        appendLine(std::to_string(logicalExprId) + " -> " + std::to_string(rightExprId)+ " [ label=\"right\" ]");
    }

    void DotfileVisitor::visitUnaryExpr(Unary* expr) {
        // print Unary stuff here
        auto unaryExprId = getExprId(expr);
        appendLine(std::to_string(unaryExprId) + " [label=\"Unary\\n"
        + to_string(expr->op()) + "\\n"
        + getType(expr->type().get()) + "\"]");

        // visit right
        expr->right()->accept(this);
        auto rightExprId = getExprId(expr->right().get());
        appendLine(std::to_string(unaryExprId) + " -> " + std::to_string(rightExprId));
    }

    void DotfileVisitor::visitVariableExpr(Variable* expr) {
        // print Variable stuff here
        auto variableExprId = getExprId(expr);
        appendLine(std::to_string(variableExprId) + " [label=\"Variable\\n"
        + expr->name() + "\\n"
        + getType(expr->type().get())+ "\"]");

        /*
        // visit resolvesTo
        auto resolvesToId = getStmtId(expr->resolvesTo());
        appendLine(std::to_string(variableExprId) + " -> " + std::to_string(resolvesToId)
        + "[ constraint=false, style=\"dotted\"]");
        */
    }

    // Literals
    void DotfileVisitor::visitNumberLiteral(NumberLiteral* expr) {
        // print NumberLiteral stuff here
        auto numberLiteralId = getExprId(expr);
        appendLine(std::to_string(numberLiteralId) + " [label=\"NumberLiteral\\n"
        + "value: \\n" +
        + "i64: " + std::to_string(expr->i64()) + "\\n"
        + "u64: " + std::to_string(expr->u64()) + "\\n"
        + "f64: " + std::to_string(expr->f64()) + "\\n"
        + getType(expr->type().get()) +"\"]");
    }

    void DotfileVisitor::visitBoolLiteral(BoolLiteral* expr) {
        // print BoolLiteral stuff here
        auto boolLiteralId = getExprId(expr);
        appendLine(std::to_string(boolLiteralId) + " [label=\"BoolLiteral\\n"
        + "value: " + to_string(expr->value()) + "\\n"
        + getType(expr->type().get())+ "\"]");
    }

    void DotfileVisitor::visitStringLiteral(StringLiteral* expr) {
        // print StringLiteral stuff here
        auto stringLiteralId = getExprId(expr);
        appendLine(std::to_string(stringLiteralId) + " [label=\"StringLiteral\\n"
        + "value: " + expr->value() + "\\n"
        + getType(expr->type().get())+ "\"]");
    }

    void DotfileVisitor::visitCharacterLiteral(CharacterLiteral* expr) {
        // print CharacterLiteral stuff here
        auto boolLiteralId = getExprId(expr);
        appendLine(std::to_string(boolLiteralId) + " [label=\"CharLiteral\\n"
        + "value: '" + std::to_string(expr->value()) + "\\n"
        + getType(expr->type().get())+ "\"]");
    }

    // Types
    void DotfileVisitor::visitFunctionType(FunctionType* type) {
        std::string str = "(";
        for (auto param : type->paramTypes()) {
            param->accept(this);
            str += _typeOfLastExpr;
            if (param != type->paramTypes()[type->paramTypes().size() - 1]) {
                str += ", ";
            }
        }
        str += ") -&#62; ";
        type->returnType()->accept(this);
        str += _typeOfLastExpr;
        _typeOfLastExpr = str;
    }

    void DotfileVisitor::visitPrimitiveType(PrimitiveType *type) {
        switch (type->type()) {
            case PrimitiveTypeKind::VOID:
                _typeOfLastExpr = "void";
                break;
            case PrimitiveTypeKind::STRING:
                _typeOfLastExpr = "string";
                break;
            case PrimitiveTypeKind::BOOL:
                _typeOfLastExpr = "bool";
                break;
            case PrimitiveTypeKind::I8:
                _typeOfLastExpr = "i8";
                break;
            case PrimitiveTypeKind::I16:
                _typeOfLastExpr = "i16";
                break;
            case PrimitiveTypeKind::I32:
                _typeOfLastExpr = "i32";
                break;
            case PrimitiveTypeKind::I64:
                _typeOfLastExpr = "i64";
                break;
            case PrimitiveTypeKind::U8:
                _typeOfLastExpr = "u8";
                break;
            case PrimitiveTypeKind::U16:
                _typeOfLastExpr = "u16";
                break;
            case PrimitiveTypeKind::U32:
                _typeOfLastExpr = "u32";
                break;
            case PrimitiveTypeKind::U64:
                _typeOfLastExpr = "u64";
                break;
            case PrimitiveTypeKind::F32:
                _typeOfLastExpr = "f32";
                break;
            case PrimitiveTypeKind::F64:
                _typeOfLastExpr = "f64";
                break;
            default:
                _typeOfLastExpr = "UNDEFINED SIMPLE TYPE";
        }
    }

    void DotfileVisitor::visitPointerType(PointerType *type) {
        std::string str = "ptr&#60;";
        type->pointsTo()->accept(this);
        str += _typeOfLastExpr + "&#62;";
        _typeOfLastExpr = str;
    }

    void DotfileVisitor::visitSimpleType(SimpleType *type) {
        // TODO: implement this
        _typeOfLastExpr = type->name();
    }

    void DotfileVisitor::reset() {
        _counter = 0;
        _moduleToId.clear();
        _stmtToId.clear();
        _exprToId.clear();
        _typeOfLastExpr = "";
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

    void DotfileVisitor::appendLine(const std::string& append) {
        _output += append + "\n";
    }

    std::string DotfileVisitor::getType(Type* type) {
        type->accept(this);
        return _typeOfLastExpr;
    }
}