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

        auto moduleId = getModuleId(module);
        appendLine(std::to_string(moduleId) + " [label=\"Module\\n" + module->filename() + "\\n\"]");

        // visit statements
        for (auto& statement : module->statements()) {
            statement->accept(this);
            auto stmtId = getStmtId(statement);
            appendLine(std::to_string(moduleId) + " -> " + std::to_string(stmtId));
        }
        appendLine("}");
    }

    // Stmt
    void DotfileVisitor::visitBlockStmt(Block* stmt) {
        auto blockId = getStmtId(stmt);
        appendLine(std::to_string(blockId) + " [label=\"Block\"]");

        for (auto& statement : stmt->statements()) {
            statement->accept(this);
            auto stmtId = getStmtId(statement);
            appendLine(std::to_string(blockId) + " -> " + std::to_string(stmtId));
        }
    }

    void DotfileVisitor::visitExpressionStmt(Expression* stmt) {
        auto expressionStmtId = getStmtId(stmt);
        appendLine(std::to_string(expressionStmtId) + " [label=\"ExpressionStmt\"]");

        // visit expression
        stmt->expression()->accept(this);
        auto expressionId = getExprId(stmt->expression());
        appendLine(std::to_string(expressionStmtId) + " -> " + std::to_string(expressionId));
    }

    void DotfileVisitor::visitExtDeclStmt(ExternalDeclaration* stmt) {
        auto externalDeclStmtId = getStmtId(stmt);
        appendLine(std::to_string(externalDeclStmtId) + " [label=\"ExternalDeclaration\\n" + stmt->name() + "\\n"
        + getType(stmt->type())+ "\"]");
    }

    void DotfileVisitor::visitImportStmt(Import* stmt) {
        auto importStmtId = getStmtId(stmt);
        appendLine(std::to_string(importStmtId) + " [label=\"Import\\n" + stmt->path() + "\"]");
    }

    void DotfileVisitor::visitFunctionStmt(Function* stmt) {
        auto functionStmtId = getStmtId(stmt);
        appendLine(std::to_string(functionStmtId) + " [label=\"Function\\n" + stmt->name() + "\\n"
        + getType(stmt->functionType())+ "\"]");

        for (auto& param : stmt->params()) {
            param->accept(this);
            auto paramStmtId = getStmtId(param);
            appendLine(std::to_string(functionStmtId) + " -> " + std::to_string(paramStmtId));
        }

        // visit FunctionBody
        for (auto& statement : stmt->body()) {
            statement->accept(this);
            auto stmtId = getStmtId(statement);
            appendLine(std::to_string(functionStmtId) + " -> " + std::to_string(stmtId));
        }
    }

    void DotfileVisitor::visitParameterStmt(Parameter* stmt) {
        auto parameterStmtId = getStmtId(stmt);
        appendLine(std::to_string(parameterStmtId) + " [label=\"Parameter\\n" + stmt->name() + "\\n"
        + getType(stmt->type())+ "\"]");
    }

    void DotfileVisitor::visitIfStmt(If* stmt) {
        auto ifStmtId = getStmtId(stmt);
        appendLine(std::to_string(ifStmtId) + " [label=\"If\"]");

        // visit condition
        stmt->condition()->accept(this);
        auto conditionExprId = getExprId(stmt->condition());
        appendLine(std::to_string(ifStmtId) + " -> " + std::to_string(conditionExprId)+ " [ label=\"condition\" ]");

        // visit then
        stmt->thenBranch()->accept(this);
        auto thenBranchStmtId = getStmtId(stmt->thenBranch());
        appendLine(std::to_string(ifStmtId) + " -> " + std::to_string(thenBranchStmtId)+ " [ label=\"then\" ]");

        // visit else
        if (stmt->elseBranch() != nullptr) {
            stmt->elseBranch()->accept(this);
            auto elseBranchStmtId = getStmtId(stmt->elseBranch());
            appendLine(std::to_string(ifStmtId) + " -> " + std::to_string(elseBranchStmtId)+ " [ label=\"else\" ]");
        }
    }

    void DotfileVisitor::visitReturnStmt(Return* stmt) {
        auto returnStmtId = getStmtId(stmt);
        appendLine(std::to_string(returnStmtId) + " [label=\"Return\"]");

        // visit value expression
        stmt->value()->accept(this);
        auto valueExprId = getExprId(stmt->value());
        appendLine(std::to_string(returnStmtId) + " -> " + std::to_string(valueExprId));
    }

    void DotfileVisitor::visitVarDeclStmt(VariableDeclaration* stmt) {
        auto varDeclStmtId = getStmtId(stmt);
        appendLine(std::to_string(varDeclStmtId) + " [label=\"VariableDeclaration\\n" + stmt->name() + "\\n"
        + "isPublic: " + to_string(stmt->isPublic()) + "\\n"
        + "isConst: " + to_string(stmt->isConst()) + "\\n"
        + getType(stmt->type())+ "\"]");

        // visit initializer
        if (stmt->initializer()) {
            stmt->initializer()->accept(this);
            auto initializerExprId = getExprId(stmt->initializer());
            appendLine(std::to_string(varDeclStmtId) + " -> " + std::to_string(initializerExprId));
        }
    }
	
	void DotfileVisitor::visitStructDeclStmt(StructDeclaration* stmt) {
		auto structDeclStmtId = getStmtId(stmt);
		appendLine(std::to_string(structDeclStmtId) + " [label=\"StructDeclaration\\n" + stmt->name() + "\\n"
			+ "isPublic: " + to_string(stmt->isPublic()) + "\"]");

		for (auto& value : stmt->members()) {
			value->accept(this);
			auto memberId = getStmtId(value);
			appendLine(std::to_string(structDeclStmtId) + " -> " + std::to_string(memberId));
		}
	}

    void DotfileVisitor::visitUnionDeclStmt(UnionDeclaration* stmt) {
        auto unionDeclStmtId = getStmtId(stmt);
        appendLine(std::to_string(unionDeclStmtId) + " [label=\"UnionDeclaration\\n" + stmt->name() + "\\n"
                   + "isPublic: " + to_string(stmt->isPublic()) + "\"]");

        for (auto& value : stmt->members()) {
            value->accept(this);
            auto memberId = getStmtId(value);
            appendLine(std::to_string(unionDeclStmtId) + " -> " + std::to_string(memberId));
        }
    }

	void DotfileVisitor::visitEnumDeclStmt(EnumDeclaration* stmt) {
		auto enumDeclStmtId = getStmtId(stmt);
		appendLine(std::to_string(enumDeclStmtId) + " [label=\"EnumDeclaration\\n" + stmt->name() + "\\n"
			+ "isPublic: " + to_string(stmt->isPublic()) + "\"]");

		// TODO: maybe print the enum values here
	}

	void DotfileVisitor::visitCustomMemberStmt(CustomMember* stmt) {
		auto memberStmtId = getStmtId(stmt);
		appendLine(std::to_string(memberStmtId) + " [label=\"CustomMember\\n" + stmt->name() + "\\n"
			+ "Type: " + getType(stmt->type()) + "\"]");
	}

    void DotfileVisitor::visitWhileStmt(While* stmt) {
        auto whileStmtId = getStmtId(stmt);
        appendLine(std::to_string(whileStmtId) + " [label=\"While\"]");

        // visit condition
        stmt->condition()->accept(this);
        auto conditionExprId = getExprId(stmt->condition());
        appendLine(std::to_string(whileStmtId) + " -> " + std::to_string(conditionExprId) + " [ label=\"condition\" ]");
        // visit body
        stmt->body()->accept(this);
        auto bodyStmtId = getStmtId(stmt->body());
        appendLine(std::to_string(whileStmtId) + " -> " + std::to_string(bodyStmtId)+ " [ label=\"body\" ]");
    }

    void DotfileVisitor::visitForStmt(For* stmt) {
        auto forStmtId = getStmtId(stmt);
        appendLine(std::to_string(forStmtId) + " [label=\"For\"]");

        // visit initializer
        if (stmt->initializer() != nullptr) {
            stmt->initializer()->accept(this);
            auto initializerStmtId = getStmtId(stmt->initializer());
            appendLine(std::to_string(forStmtId) + " -> " + std::to_string(initializerStmtId) + " [ label=\"initializer\" ]");
        }
        // visit condition
        stmt->condition()->accept(this);
        auto conditionExprId = getExprId(stmt->condition());
        appendLine(std::to_string(forStmtId) + " -> " + std::to_string(conditionExprId) + " [ label=\"condition\" ]");

        // visit increment
        if (stmt->increment() != nullptr) {
            stmt->increment()->accept(this);
            auto incrementExprId = getExprId(stmt->increment());
            appendLine(std::to_string(forStmtId) + " -> " + std::to_string(incrementExprId) + " [ label=\"increment\" ]");
        }

        // visit body
        stmt->body()->accept(this);
        auto bodyStmtId = getStmtId(stmt->body());
        appendLine(std::to_string(forStmtId) + " -> " + std::to_string(bodyStmtId)+ " [ label=\"body\" ]");
    }

    void DotfileVisitor::visitBreakStmt(Break* stmt) {
        auto breakStmtId = getStmtId(stmt);
        appendLine(std::to_string(breakStmtId) + " [label=\"break\"]");
    }

    void DotfileVisitor::visitContinueStmt(Continue* stmt) {
        auto continueStmtId = getStmtId(stmt);
        appendLine(std::to_string(continueStmtId) + " [label=\"continue\"]");
    }

    void DotfileVisitor::visitDeferStmt(Defer* stmt) {
        auto deferStmtId = getStmtId(stmt);
        appendLine(std::to_string(deferStmtId) + " [label=\"defer\"]");

        stmt->stmtToDefer()->accept(this);
        auto stmtToDeferId = getStmtId(stmt->stmtToDefer());
        appendLine(std::to_string(deferStmtId) + " -> " + std::to_string(stmtToDeferId));
    }

    void DotfileVisitor::visitCommentStmt(Comment* stmt) {
        auto commentStmtId = getStmtId(stmt);
        appendLine(std::to_string(commentStmtId) + " [label=\"Comment\\n" + stmt->text() + "\"]");
    }

    // Expr
    void DotfileVisitor::visitAssignExpr(Assign* expr) {
        auto assignmentExprId = getExprId(expr);
        appendLine(std::to_string(assignmentExprId) + " [label=\"Assignment\\n"+ getType(expr->type()) + "\"]");

        // visit target
		if (expr->target()) {
			expr->target()->accept(this);
			auto targetId = getExprId(expr->target());
			appendLine(std::to_string(assignmentExprId) + " -> " + std::to_string(targetId) + " [ label=\"target\" ]");
		}

		if (expr->targetExpr()) {
			expr->targetExpr()->accept(this);
			auto targetExprId = getExprId(expr->targetExpr());
			appendLine(std::to_string(assignmentExprId) + " -> " + std::to_string(targetExprId) + " [ label=\"targetExpr\" ]");
		}

        // visit value
        expr->value()->accept(this);
        auto valueExprId = getExprId(expr->value());
        appendLine(std::to_string(assignmentExprId) + " -> " + std::to_string(valueExprId)+ " [ label=\"value\" ]");
    }

    void DotfileVisitor::visitBinaryExpr(Binary* expr) {
        auto binaryExprId = getExprId(expr);
        appendLine(std::to_string(binaryExprId) + " [label=\"Binary\\n"
        + to_string(expr->op()) + "\\n" +
        getType(expr->type())+ "\"]");

        // visit left
        expr->left()->accept(this);
        auto leftExprId = getExprId(expr->left());
        appendLine(std::to_string(binaryExprId) + " -> " + std::to_string(leftExprId)+ " [ label=\"left\" ]");

        // visit right
        expr->right()->accept(this);
        auto rightExprId = getExprId(expr->right());
        appendLine(std::to_string(binaryExprId) + " -> " + std::to_string(rightExprId)+ " [ label=\"right\" ]");
    }

    void DotfileVisitor::visitCallExpr(Call* expr) {
        auto callExprId = getExprId(expr);
        appendLine(std::to_string(callExprId) + " [label=\"Call\\n" + getType(expr->type()) + "\"]");

        // visit callee
        expr->callee()->accept(this);
        auto calleeId = getExprId(expr->callee());
        appendLine(std::to_string(callExprId) + " -> " + std::to_string(calleeId) + "[ label=\"callee\" ]");

        // visit args
        for (auto& arg : expr->args()) {
            arg->accept(this);
            auto argId = getExprId(arg);
            appendLine(std::to_string(callExprId) + " -> " + std::to_string(argId));
        }
    }

    void DotfileVisitor::visitGroupingExpr(Grouping* expr) {
        auto groupingExprId = getExprId(expr);
        appendLine(std::to_string(groupingExprId) + " [label=\"Grouping\\n" + getType(expr->type()) + "\"]");

        // visit expression
        expr->expression()->accept(this);
        auto exprId = getExprId(expr->expression());
        appendLine(std::to_string(groupingExprId) + " -> " + std::to_string(exprId));
    }

	void DotfileVisitor::visitSubscriptExpr(Subscript* expr) {
		auto subscriptExprId = getExprId(expr);
		appendLine(std::to_string(subscriptExprId) + " [label=\"Subscript\\n" + getType(expr->type()) + "\"]");

		// visit target
		expr->target()->accept(this);
		auto targetExprId = getExprId(expr->target());
		appendLine(std::to_string(subscriptExprId) + " -> " + std::to_string(targetExprId));

		// visit index
		expr->index()->accept(this);
		auto indexExprId = getExprId(expr->index());
		appendLine(std::to_string(subscriptExprId) + " -> " + std::to_string(indexExprId));
	}

	void DotfileVisitor::visitMemberAccessExpr(MemberAccess *expr) {
        auto memberAccessExprId = getExprId(expr);
        appendLine(std::to_string(memberAccessExprId) + " [label=\"MemberAccess\\n"
        + expr->value() + "\\n"
        + getType(expr->type()) + "\"]");

        // visit target
        expr->target()->accept(this);
        auto targetExprId = getExprId(expr->target());
        appendLine(std::to_string(memberAccessExprId) + " -> " + std::to_string(targetExprId));
    }
	
	void DotfileVisitor::visitEnumAccessExpr(EnumAccess* expr) {
		auto scopeAccessExprId = getExprId(expr);
		appendLine(std::to_string(scopeAccessExprId) + " [label=\"EnumAccess\\n"
			+ expr->value() + "\\n"
			+ getType(expr->type()) + "\"]");
	}

    void DotfileVisitor::visitLogicalExpr(Logical* expr) {
        auto logicalExprId = getExprId(expr);
        appendLine(std::to_string(logicalExprId) + " [label=\"Logical\\n"
        + to_string(expr->op()) + "\\n"
        + getType(expr->type())+ "\"]");

        // visit left
        expr->left()->accept(this);
        auto leftExprId = getExprId(expr->left());
        appendLine(std::to_string(logicalExprId) + " -> " + std::to_string(leftExprId)+ " [ label=\"left\" ]");

        // visit right
        expr->right()->accept(this);
        auto rightExprId = getExprId(expr->right());
        appendLine(std::to_string(logicalExprId) + " -> " + std::to_string(rightExprId)+ " [ label=\"right\" ]");
    }

    void DotfileVisitor::visitUnaryExpr(Unary* expr) {
        auto unaryExprId = getExprId(expr);
        appendLine(std::to_string(unaryExprId) + " [label=\"Unary\\n"
        + to_string(expr->op()) + "\\n"
        + getType(expr->type()) + "\"]");

        // visit right
        expr->right()->accept(this);
        auto rightExprId = getExprId(expr->right());
        appendLine(std::to_string(unaryExprId) + " -> " + std::to_string(rightExprId));
    }

    void DotfileVisitor::visitSizeOfExpr(SizeOf *expr) {
        auto sizeOfExprId = getExprId(expr);
        appendLine(std::to_string(sizeOfExprId) + " [label=\"sizeof\\n"
        + getType(expr->right()) + "\\n"
        + getType(expr->type()) + "\"]");
    }

    void DotfileVisitor::visitCastExpr(Cast* expr) {
        auto castExprId = getExprId(expr);
        appendLine(std::to_string(castExprId) + " [label=\"cast\\n"
                   + getType(expr->targetType()) + "\"]");

        // visit right
        expr->right()->accept(this);
        auto rightExprId = getExprId(expr->right());
        appendLine(std::to_string(castExprId) + " -> " + std::to_string(rightExprId));
    }

    void DotfileVisitor::visitVariableExpr(Variable* expr) {
        auto variableExprId = getExprId(expr);
        appendLine(std::to_string(variableExprId) + " [label=\"Variable\\n"
        + expr->name() + "\\n"
        + getType(expr->type())+ "\"]");

        /*
        // visit resolvesTo
        auto resolvesToId = getStmtId(expr->resolvesTo());
        appendLine(std::to_string(variableExprId) + " -> " + std::to_string(resolvesToId)
        + "[ constraint=false, style=\"dotted\"]");
        */
    }

    // Literals
    void DotfileVisitor::visitNumberLiteral(NumberLiteral* expr) {
        auto numberLiteralId = getExprId(expr);
        appendLine(std::to_string(numberLiteralId) + " [label=\"NumberLiteral\\n"
        + "value: \\n" +
        + "i64: " + std::to_string(expr->i64()) + "\\n"
        + "u64: " + std::to_string(expr->u64()) + "\\n"
        + "f64: " + std::to_string(expr->f64()) + "\\n"
        + getType(expr->type()) +"\"]");
    }

    void DotfileVisitor::visitBoolLiteral(BoolLiteral* expr) {
        auto boolLiteralId = getExprId(expr);
        appendLine(std::to_string(boolLiteralId) + " [label=\"BoolLiteral\\n"
        + "value: " + to_string(expr->value()) + "\\n"
        + getType(expr->type())+ "\"]");
    }

    void DotfileVisitor::visitStringLiteral(StringLiteral* expr) {
        auto stringLiteralId = getExprId(expr);
        appendLine(std::to_string(stringLiteralId) + " [label=\"StringLiteral\\n"
        + "value: " + expr->value() + "\\n"
        + getType(expr->type())+ "\"]");
    }

    void DotfileVisitor::visitCharacterLiteral(CharacterLiteral* expr) {
        auto charLiteralId = getExprId(expr);
        appendLine(std::to_string(charLiteralId) + " [label=\"CharLiteral\\n"
        + "value: '" + std::to_string(expr->value()) + "\\n"
        + getType(expr->type())+ "\"]");
    }

    void DotfileVisitor::visitArrayLiteral(ArrayLiteral* expr) {
        auto arrayLiteralId = getExprId(expr);
        appendLine(std::to_string(arrayLiteralId) + " [label=\"ArrayLiteral\\n"
        + getType(expr->type())+ "\"]");

		for (auto& value : expr->values()) {
			// visit target
			value->accept(this);
			auto valueExprId = getExprId(value);
			appendLine(std::to_string(arrayLiteralId) + " -> " + std::to_string(valueExprId));
		}
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
        std::string str;
        if (type->isArray()) {
            str += "[";
            type->pointsTo()->accept(this);
            str += _typeOfLastExpr;
            str += ", " + std::to_string(type->size()) + "]";

        } else {
            str += "ptr&#60;";
            type->pointsTo()->accept(this);
            str += _typeOfLastExpr + "&#62;";
        }
        _typeOfLastExpr = str;
    }

    void DotfileVisitor::visitCustomType(CustomType *type) {
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