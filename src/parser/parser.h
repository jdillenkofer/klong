#pragma once

#include <string>
#include <memory>

#include "iparser.h"
#include "../lexer/ilexer.h"
#include "type.h"
#include "module.h"
#include "stmt.h"
#include "expr.h"

#include "visitor.h" // <-- TODO: remove this include

namespace klong {
    class Parser {
        public:
            Parser(ILexer* lexer):
                _lexer(lexer) {
                advance();
            }

            ModulePtr parse();
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
            std::shared_ptr<Function> function(const std::string& kind, bool isPublic = false);
            std::vector<StmtPtr> blockStmt();
            std::shared_ptr<VariableDeclaration> letDeclaration(bool isPublic = false);
            std::shared_ptr<VariableDeclaration> constDeclaration(bool isPublic = false);
            TypePtr typeDeclaration();
            std::shared_ptr<If> ifStmt();
            std::shared_ptr<Print> printStmt();
            std::shared_ptr<Return> returnStmt();
            std::shared_ptr<While> whileStmt();
            std::shared_ptr<For> forStmt();
            std::shared_ptr<Expression> expressionStmt();
            StmtPtr statement();
            ExprPtr expression();
            ExprPtr assignmentExpr();
            ExprPtr orExpr();
            ExprPtr andExpr();
            ExprPtr equalityExpr();
            ExprPtr comparisonExpr();
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
    };
}