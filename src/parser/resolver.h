#pragma once

#include <stack>
#include <map>

#include "visitor.h"
#include "stmt.h"
#include "expr.h"

namespace klong {
    class ResolveException : public std::exception {
        public:
            ResolveException(SourceRange sourceRange, std::string message):
                    _sourceRange(sourceRange), _message(message) {

            }

            SourceRange sourceRange() const {
                return _sourceRange;
            }

            const char* what () const throw () {
                return _message.c_str();
            }

        private:
            SourceRange _sourceRange;
            std::string _message;
    };

    enum class DeclarationType {
        CONST,
        LET,
        PARAM_DECL,
        FUNCTION_DECL
    };

    struct SymbolInfo {
        Stmt* declarationStmt;
        DeclarationType declarationType;
        bool initialized = false;
    };

    class Resolver : public Visitor {
        public:
            Resolver() = default;

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
            void visitCommentStmt(Comment* expr) override;

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
            void resolve(const StmtPtr& stmt);
            void resolve(const ExprPtr& expr);

            void resolveLocal(Variable* variable);

            void resolveFunction(Function* stmt, bool insideFunction);

            void enterScope();
            void exitScope();

            void declare(Stmt* declarationStmt, std::string name, DeclarationType declarationType);
            void define(std::string name);

        private:
            std::deque<std::map<std::string, SymbolInfo>> _scopes;
            bool _isInsideFunction = false;
    };
}