#pragma once

#include <any>
#include <string>
#include <exception>
#include "../parser/module.h"
#include "../parser/stmt.h"
#include "../parser/expr.h"
#include "../parser/visitor.h"
#include "terp_exception.h"
#include "environment.h"

namespace klong {
    class ReturnWrapper : public std::exception {
        public:
            ReturnWrapper(std::any value): _value(std::move(value)) {

            }

            std::any value() const {
                return _value;
            }

        private:
            std::any _value;
    };

    class TreewalkTerp : public Visitor {
        public:
            TreewalkTerp() = default;

            // Module
            void visitModule(Module* module) override;

            // Stmt
            void visitBlockStmt(Block* stmt) override;
            void visitExpressionStmt(Expression* stmt) override;
            void visitFunctionStmt(Function* stmt) override;
            void visitParameterStmt(Parameter* stmt) override;
            void visitIfStmt(If* stmt) override;
            void visitPrintStmt(Print* stmt) override;
            void visitReturnStmt(Return* stmt) override;
            void visitLetStmt(Let* stmt) override;
            void visitConstStmt(Const* stmt) override;
            void visitWhileStmt(While* stmt) override;
            void visitForStmt(For* stmt) override;
            void visitCommentStmt(Comment* stmt) override;

            // Expr
            void visitAssignExpr(Assign* expr) override;
            void visitBinaryExpr(Binary* expr) override;
            void visitCallExpr(Call* expr) override;
            void visitGroupingExpr(Grouping* expr) override;
            void visitLogicalExpr(Logical* expr) override;
            void visitUnaryExpr(Unary* expr) override;
            void visitVariableExpr(Variable* expr) override;

            // Literals
            void visitNumberLiteral(NumberLiteral* expr) override;
            void visitBoolLiteral(BoolLiteral* expr) override;
            void visitStringLiteral(StringLiteral* expr) override;
            void visitCharacterLiteral(CharacterLiteral* expr) override;

            // Types
            void visitFunctionType(FunctionType* type) override;
            void visitPrimitiveType(PrimitiveType *type) override;
            void visitSimpleType(SimpleType *type) override;
        private:
            std::any evaluate(Expr* expr);
            void execute(Stmt* stmt);
            void executeBlock(const std::vector<StmtPtr>& statements, std::shared_ptr<Environment> env);
            bool isTruthy(std::any value);
            std::string stringify(std::any value);
        private:
            std::any _valueOfLastExpr;
            std::shared_ptr<Environment> _globals = std::make_shared<Environment>();
            std::shared_ptr<Environment> _environment = _globals;
    };
}