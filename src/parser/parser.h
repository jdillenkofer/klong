#pragma once

#include <string>
#include <memory>

#include "common/compilation_session.h"
#include "lexer/lexer.h"
#include "ast/type.h"
#include "ast/module.h"
#include "ast/stmt.h"
#include "ast/expr.h"
#include "parser_memento.h"


namespace klong {
    class Parser {
    public:
        explicit Parser(Lexer* lexer, CompilationSession* session):
            _lexer(lexer),
            _session(session) {
            advance();
        }

        void parse();

        ParserMemento saveToMemento();
        void loadFromMemento(ParserMemento& memento);
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
        std::shared_ptr<Function> function(bool isPublic = false);
        std::vector<StmtPtr> blockStmt();
        std::shared_ptr<VariableDeclaration> letDeclaration(bool isPublic = false);
        std::shared_ptr<VariableDeclaration> constDeclaration(bool isPublic = false);
		std::shared_ptr<MemberTypeDeclaration> memberTypeDeclaration(bool isPublic = false);
		std::shared_ptr<EnumDeclaration> enumDeclaration(bool isPublic = false);
		std::shared_ptr<Import> import();
        TypePtr typeDeclaration();
        std::shared_ptr<If> ifStmt();
        std::shared_ptr<Return> returnStmt();
        std::shared_ptr<While> whileStmt();
        std::shared_ptr<For> forStmt();
        std::shared_ptr<Break> breakStmt();
        std::shared_ptr<Continue> continueStmt();
        std::shared_ptr<Defer> deferStmt();
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
        ExprPtr postfixExpr();
		ExprPtr finishPostfixExpr(ExprPtr lhs);
		ExprPtr finishCallExpr(ExprPtr callee);
		ExprPtr finishSubscriptExpr(ExprPtr target);
		ExprPtr finishMemberAccessExpr(ExprPtr target);
        ExprPtr primary();
        ExprPtr literal();
    private:
        Token _current;
        Token _previous;
        Lexer* _lexer;
        std::shared_ptr<Module> _module;
        bool _isInsideFunction = false;
        bool _isInsideLoop = false;
        bool _isInsideDefer = false;
        CompilationSession* _session;
    };
}