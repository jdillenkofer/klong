#pragma once

#include <string>
#include <memory>
#include <array>

#include "iparser.h"
#include "../lexer/ilexer.h"
#include "expr.h"

#include "visitor.h" // <-- TODO: remove this include

namespace klong {
    class Parser {
        public:
            Parser(ILexer* lexer):
                _lexer(lexer) {
                advance();
            }

            std::vector<StmtPtr> parse();
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

            StmtPtr declarationStmt();
            std::shared_ptr<Function> function(std::string kind);
            std::vector<StmtPtr> blockStmt();
            std::shared_ptr<Let> letDeclaration();
            std::shared_ptr<Const> constDeclaration();
            std::shared_ptr<If> ifStmt();
            std::shared_ptr<Print> printStmt();
            std::shared_ptr<Return> returnStmt();
            std::shared_ptr<While> whileStmt();
            StmtPtr forStmt();
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
    };
}