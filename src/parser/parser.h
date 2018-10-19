#pragma once

#include <string>
#include <memory>

#include "iparser.h"
#include "../lexer/ilexer.h"
#include "ast/type.h"
#include "ast/module.h"
#include "ast/stmt.h"
#include "ast/expr.h"


namespace klong {
    class Parser : public IParser {
    public:
        explicit Parser(ILexer* lexer):
            _lexer(lexer) {
            advance();
        }

        Result<ModulePtr, ParseException> parse() override;
    private:
        template<typename... T> bool match(T... types) {
            for (auto& type : { types... }) {
                if (check(type)) {
                    advance();
                    return true;
                }
            }
            return false;
        }

        Token consume(TokenType type, std::string errorMessage);
        bool check(TokenType type);
        Token peek();
        Token previous();
        Token advance();
        bool isAtEnd();
        void synchronize();
        std::shared_ptr<PrimitiveType> getPrimitiveTypefromToken(Token token);

        StmtPtr declarationStmt();
        std::shared_ptr<ExternalDeclaration> externDeclStmt();
        std::shared_ptr<Function> function(const std::string& kind, bool isPublic = false);
        std::vector<StmtPtr> blockStmt();
        std::shared_ptr<VariableDeclaration> letDeclaration(bool isPublic = false);
        std::shared_ptr<VariableDeclaration> constDeclaration(bool isPublic = false);
        TypePtr typeDeclaration();
        std::shared_ptr<If> ifStmt();
        std::shared_ptr<Return> returnStmt();
        std::shared_ptr<While> whileStmt();
        std::shared_ptr<For> forStmt();
        std::shared_ptr<Break> breakStmt();
        std::shared_ptr<Continue> continueStmt();
        std::shared_ptr<Expression> expressionStmt();
        StmtPtr statement();
        ExprPtr expression();
        ExprPtr assignmentExpr();
        ExprPtr bitwiseAnd();
        ExprPtr bitwiseXOr();
        ExprPtr bitwiseOr();
        ExprPtr logicalOrExpr();
        ExprPtr logicalAndExpr();
        ExprPtr equalityExpr();
        ExprPtr comparisonExpr();
        ExprPtr shiftExpr();
        ExprPtr additionExpr();
        ExprPtr multiplicationExpr();
        ExprPtr unaryExpr();
        ExprPtr finishCallExpr(ExprPtr callee);
        ExprPtr callExpr();
        ExprPtr primary();
    private:
        Token _current;
        Token _previous;
        ILexer* _lexer;
        bool _isInsideFunction = false;
        bool _isInsideLoop = false;
        std::vector<ParseException> _errors;
    };
}