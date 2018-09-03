#include "parser.h"

#include <exception>

namespace klong {
    std::vector<StmtPtr> Parser::parse() {
        std::vector<StmtPtr> statements;
        while(!isAtEnd()) {
            statements.push_back(std::move(declarationStmt()));
        }
        return statements;
    }

    Token Parser::consume(TokenType type, std::string errorMessage) {
        if (check(type)) {
            return advance();
        }

        throw ParseException(peek(), errorMessage);
    }

    bool Parser::check(TokenType type) {
        if(isAtEnd()) {
            return false;
        }
        return peek().type == type;
    }

    Token Parser::peek() {
        return _current;
    }

    Token Parser::previous() {
        return _previous;
    }

    Token Parser::advance() {
        if(!isAtEnd()) {
            _previous = _current;
            _current = _lexer->next();
        }
        return previous();
    }

    bool Parser::isAtEnd() {
        return peek().type == TokenType::END_OF_FILE;
    }

    StmtPtr Parser::declarationStmt() {
        if (match(TokenType::FUN)) {
            return functionStmt("function");
        }

        if (match(TokenType::LET)) {
            return letStmt();
        }

        if (match(TokenType::CONST)) {
            return constStmt();
        }
        throw ParseException(peek(), "Error");
    }
    
    std::shared_ptr<Function> Parser::functionStmt(std::string kind) {
        Token name = consume(TokenType::IDENTIFIER, "Expect " + kind + " name.");
        consume(TokenType::LEFT_PAR, "Expected '(' after " + kind + " name.");
        std::vector<Token> params;
        if (!check(TokenType::RIGHT_PAR)) {
            do {
                params.push_back(consume(TokenType::IDENTIFIER, "Expect parameter name."));
            } while(match(TokenType::COMMA));
        }
        consume(TokenType::RIGHT_PAR, "Expect ')' after parameters.");
        consume(TokenType::LEFT_CURLY_BRACE, "Expect '{' before " + kind + " body.");
        std::vector<StmtPtr> body = blockStmt();
        return std::make_shared<Function>(name, params, body);
    }

    std::vector<StmtPtr> Parser::blockStmt() {
        std::vector<StmtPtr> statments;
        while(!check(TokenType::RIGHT_CURLY_BRACE) && !isAtEnd()) {
            statments.push_back(declarationStmt());
        }
        consume(TokenType::RIGHT_CURLY_BRACE, "Expect '}' after block.");
        return statments;
    }

    std::shared_ptr<Let> Parser::letStmt() {
        Token name = consume(TokenType::IDENTIFIER, "Expect variable name.");

        ExprPtr initializer = nullptr;
        /*
        if (match(TokenType::ASSIGN_OP)) {
            initializer = expression();
        }
        */

        consume(TokenType::SEMICOLON, "Expect ';' after let statement.");
        return std::make_shared<Let>(name, initializer);
    }

    std::shared_ptr<Const> Parser::constStmt() {
        Token name = consume(TokenType::IDENTIFIER, "Expect variable name.");

        ExprPtr initializer = nullptr;
        /*
        if (match(TokenType::ASSIGN_OP)) {
            initializer = expression();
        }
        */

        consume(TokenType::SEMICOLON, "Expect ';' after let statement.");
        return std::make_shared<Const>(name, initializer);
    }

    std::shared_ptr<If> Parser::ifStmt() {
        consume(TokenType::LEFT_PAR, "Expect '(' after 'if'.");
        ExprPtr condition = expression();
        consume(TokenType::RIGHT_PAR, "Expect ')' after condition.");

        StmtPtr thenBranch = statement();
        StmtPtr elseBranch = nullptr;
        if (match(TokenType::ELSE)) {
            elseBranch = statement();
        }

        return std::make_shared<If>(condition, thenBranch, elseBranch);
    }

    std::shared_ptr<Print> Parser::printStmt() {
        ExprPtr value = expression();
        consume(TokenType::SEMICOLON, "Expect ';' after value.");
        return std::make_shared<Print>(value);
    }

    std::shared_ptr<Return> Parser::returnStmt() {
        Token keyword = previous();
        ExprPtr value = nullptr;
        if (!check(TokenType::SEMICOLON)) {
            value = expression();
        }

        consume(TokenType::SEMICOLON, "Expect ';' after return value.");
        return std::make_shared<Return>(keyword, value);
    }

    std::shared_ptr<While> Parser::whileStmt() {
        consume(TokenType::LEFT_PAR, "Expect '(' after 'while'.");
        ExprPtr condition = expression();
        consume(TokenType::RIGHT_PAR, "Expect ')' after condition.");
        StmtPtr body = statement();
        return std::make_shared<While>(condition, body);
    }

    std::shared_ptr<Expression> Parser::expressionStmt() {
        ExprPtr expr = expression();
        consume(TokenType::SEMICOLON, "Expect ';' after expression.");
        return std::make_shared<Expression>(expr);
    }

    StmtPtr Parser::statement() {
        if (match(TokenType::IF)) {
            return ifStmt();
        }
        if (match(TokenType::PRINT)) {
            return printStmt();
        }
        if (match(TokenType::RETURN)) {
            return returnStmt();
        }
        if (match(TokenType::WHILE)) {
            return whileStmt();
        }
        if (match(TokenType::LEFT_CURLY_BRACE)) {
            return std::make_shared<Block>(blockStmt());
        }
        return expressionStmt();
    }

    ExprPtr Parser::expression() {
        return assignment();
    }

    std::shared_ptr<Assign> Parser::assignment() {
        return nullptr;
/*
        ExprPtr expr = or();
        if (match(TokenType::ASSIGN_OP)) {
            Token assign = previous();
            ExprPtr value = assignment();
        }
*/
    }
}