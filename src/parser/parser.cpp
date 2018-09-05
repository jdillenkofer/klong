#include "parser.h"

#include <iostream>
#include <exception>

namespace klong {
    ModulePtr Parser::parse() {
        std::vector<StmtPtr> statements;
        while(!isAtEnd()) {
            statements.push_back(std::move(declarationStmt()));
        }
        return std::make_shared<Module>(std::move(statements));
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

    void Parser::synchronize() {
        advance();

        while(!isAtEnd()) {
            if (previous().type == TokenType::SEMICOLON) return;

            switch(peek().type) {
                case TokenType::FUN:
                case TokenType::LET:
                case TokenType::CONST:
                case TokenType::FOR:
                case TokenType::IF:
                case TokenType::WHILE:
                case TokenType::PRINT:
                case TokenType::RETURN:
                    return;
                default:
                    break;
            }

            advance();
        }
    }

    StmtPtr Parser::declarationStmt() {
        try {
            if (match(TokenType::FUN)) {
                return function("function");
            }

            if (match(TokenType::LET)) {
                return letDeclaration();
            }

            if (match(TokenType::CONST)) {
                return constDeclaration();
            }

            if (match(TokenType::LINE_COMMENT, TokenType::BLOCK_COMMENT)) {
                return std::make_shared<Comment>(previous());
            }

            return statement();
        } catch(const ParseException& error) {
            std::cerr << error.what() << std::endl;
            synchronize();
            return nullptr;
        }
    }
    
    std::shared_ptr<Function> Parser::function(std::string kind) {
        Token name = consume(TokenType::IDENTIFIER, "Expect " + kind + " name.");
        consume(TokenType::LEFT_PAR, "Expected '(' after " + kind + " name.");
        std::vector<Token> params;
        std::vector<TypePtr> paramTypes;
        if (!check(TokenType::RIGHT_PAR)) {
            do {
                Token identifier = consume(TokenType::IDENTIFIER, "Expect parameter name.");
                params.push_back(identifier);
                consume(TokenType::COLON, "Expect ':' after parameter name.");
                TypePtr type = typeDeclaration();
                                
                // TODO: refactor this cast
                if (type->kind() == TypeKind::PRIMITIVE) {
                    std::shared_ptr<PrimitiveType> primitiveType = std::dynamic_pointer_cast<PrimitiveType>(type);
                    if (primitiveType->token().type == TokenType::VOID) {
                        throw ParseException(primitiveType->token(), "Illegal type 'void' in argument list.");
                    }
                }
                paramTypes.push_back(type);
            } while(match(TokenType::COMMA));
        }
        consume(TokenType::RIGHT_PAR, "Expect ')' after parameters.");
        TypePtr returnType = nullptr;
        if (match(TokenType::COLON)) {
            returnType = typeDeclaration();   
        }
        consume(TokenType::LEFT_CURLY_BRACE, "Expect '{' before " + kind + " body.");
        std::vector<StmtPtr> body = blockStmt();
        auto functionType = std::make_shared<FunctionType>(std::move(paramTypes), returnType);
        return std::make_shared<Function>(name, std::move(params), functionType, std::move(body));
    }

    std::vector<StmtPtr> Parser::blockStmt() {
        std::vector<StmtPtr> statements;
        while(!check(TokenType::RIGHT_CURLY_BRACE) && !isAtEnd()) {
            statements.push_back(declarationStmt());
        }
        consume(TokenType::RIGHT_CURLY_BRACE, "Expect '}' after block.");
        return statements;
    }

    std::shared_ptr<Let> Parser::letDeclaration() {
        Token name = consume(TokenType::IDENTIFIER, "Expect variable name.");

        TypePtr type = nullptr;
        if (match(TokenType::COLON)) {
            type = typeDeclaration();
        }
        ExprPtr initializer = nullptr;
        if (match(TokenType::ASSIGN_OP)) {
            initializer = expression();
        }
        
        consume(TokenType::SEMICOLON, "Expect ';' after let statement.");
        return std::make_shared<Let>(name, type, initializer);
    }

    std::shared_ptr<Const> Parser::constDeclaration() {
        Token name = consume(TokenType::IDENTIFIER, "Expect variable name.");

        TypePtr type = nullptr;
        if (match(TokenType::COLON)) {
            type = typeDeclaration();
        }
        ExprPtr initializer = nullptr;
        if (match(TokenType::ASSIGN_OP)) {
            initializer = expression();
        }
        
        consume(TokenType::SEMICOLON, "Expect ';' after let statement.");
        return std::make_shared<Const>(name, type, initializer);
    }

    TypePtr Parser::typeDeclaration() {
        Token type = peek();
        advance();
        switch(type.type) {
            case TokenType::LEFT_PAR:
            {
                std::vector<TypePtr> argTypes;
                do {
                    argTypes.push_back(typeDeclaration());
                } while(match(TokenType::COMMA));
                consume(TokenType::RIGHT_PAR, "Expect ')' as type list terminator.");
                consume(TokenType::ARROW, "Expect '->' after type list in function type.");
                auto returnType = typeDeclaration();
                return std::make_shared<FunctionType>(std::move(argTypes), returnType);
            }
            case TokenType::VOID:
            case TokenType::STRING:
            case TokenType::BOOL:
            case TokenType::I8:
            case TokenType::I16:
            case TokenType::I32:
            case TokenType::I64:
            case TokenType::U8:
            case TokenType::U16:
            case TokenType::U32:
            case TokenType::U64:
            case TokenType::F32:
            case TokenType::F64:
            {
                return std::make_shared<PrimitiveType>(type);
            }
            case TokenType::IDENTIFIER:
            {
                return std::make_shared<SimpleType>(type);
            }
            default:
                throw new ParseException(peek(), "Expect type after ':'.");
        }
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

    std::shared_ptr<For> Parser::forStmt() {
        consume(TokenType::LEFT_PAR, "Expect '(' after 'for'.");
        StmtPtr initializer;
        if (match(TokenType::SEMICOLON)) {
            initializer = nullptr;
        } else if (match(TokenType::LET)) {
            initializer = letDeclaration();
        } else if (match(TokenType::CONST)) {
            initializer = constDeclaration();
        } else {
            initializer = expressionStmt();
        }

        ExprPtr condition = nullptr;
        if (!check(TokenType::SEMICOLON)) {
            condition = expression();
        }
        consume(TokenType::SEMICOLON, "Expect ';' after loop condition.");

        ExprPtr increment = nullptr;
        if (!check(TokenType::RIGHT_PAR)) {
            increment = expression();
        }
        consume(TokenType::RIGHT_PAR, "Expect ')' after for clause.");

        StmtPtr body = statement();
        
        if (condition == nullptr) {
            Token trueToken;
            trueToken.type = TokenType::TRUE_KEYWORD;
            condition = std::make_shared<Literal>(trueToken);
        }

        return std::make_shared<For>(initializer, condition, increment, body);
    }

    std::shared_ptr<Expression> Parser::expressionStmt() {
        ExprPtr expr = expression();
        consume(TokenType::SEMICOLON, "Expect ';' after expression.");
        return std::make_shared<Expression>(expr);
    }

    StmtPtr Parser::statement() {
        if (match(TokenType::FOR)) {
            return forStmt();
        }
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
        return assignmentExpr();
    }

    ExprPtr Parser::assignmentExpr() {
        ExprPtr expr = orExpr();
        if (match(TokenType::ASSIGN_OP)) {
            Token assign = previous();
            ExprPtr value = assignmentExpr();
            if (value->kind() == ExprKind::VARIABLE) {
                auto variable = std::dynamic_pointer_cast<Variable>(expr);
                Token name = variable->name();
                return std::make_shared<Assign>(name, value);
            }

            throw ParseException(assign, "Invalid assign target");
        }
        return expr;
    }

    ExprPtr Parser::orExpr() {
        ExprPtr expr = andExpr();

        while(match(TokenType::OR)) {
            Token op = previous();
            ExprPtr right = andExpr();
            expr = std::make_shared<Logical>(expr, op, right);
        }

        return expr;
    }

    ExprPtr Parser::andExpr() {
        ExprPtr expr = equalityExpr();

        while(match(TokenType::AND)) {
            Token op = previous();
            ExprPtr right = equalityExpr();
            expr = std::make_shared<Logical>(expr, op, right);
        }

        return expr;
    }

    ExprPtr Parser::equalityExpr() {
        ExprPtr expr = comparisonExpr();

        while(match(TokenType::EQ_OP, TokenType::NE_OP)) {
            Token op = previous();
            ExprPtr right = comparisonExpr();
            expr = std::make_shared<Binary>(expr, op, right);
        }

        return expr;
    }

    ExprPtr Parser::comparisonExpr() {
        ExprPtr expr = additionExpr();

        while(match(TokenType::GT_OP, TokenType::GE_OP, TokenType::LT_OP, TokenType::LE_OP)) {
            Token op = previous();
            ExprPtr right = additionExpr();
            expr = std::make_shared<Binary>(expr, op, right);
        }

        return expr;
    }

    ExprPtr Parser::additionExpr() {
        ExprPtr expr = multiplicationExpr();

        while (match(TokenType::MINUS, TokenType::PLUS)) {
            Token op = previous();
            ExprPtr right = multiplicationExpr();
            expr = std::make_shared<Binary>(expr, op, right);
        }

        return expr;
    }

    ExprPtr Parser::multiplicationExpr() {
        ExprPtr expr = unaryExpr();

        while(match(TokenType::SLASH, TokenType::ASTERISK)) {
            Token op = previous();
            ExprPtr right = unaryExpr();
            expr = std::make_shared<Binary>(expr, op, right);
        }

        return expr;
    }

    ExprPtr Parser::unaryExpr() {
        if (match(TokenType::BANG, TokenType::MINUS)) {
            Token op = previous();
            ExprPtr right = unaryExpr();
            return std::make_shared<Unary>(op, right);
        }

        return callExpr();
    }

    ExprPtr Parser::finishCallExpr(ExprPtr callee) {
        std::vector<ExprPtr> args;
        if (!check(TokenType::RIGHT_PAR)) {
            do {
                args.push_back(expression());
            } while(match(TokenType::COMMA));
        }
        Token paren = consume(TokenType::RIGHT_PAR, "Expect ')' after arguments.");

        return std::make_shared<Call>(callee, paren, std::move(args));
    }

    ExprPtr Parser::callExpr() {
        ExprPtr expr = primary();

        while(true) {
            if (match(TokenType::LEFT_PAR)) {
                expr = finishCallExpr(expr);
            } else {
                break;
            }
        }

        return expr;
    }

    ExprPtr Parser::primary() {
        if (match(TokenType::FALSE_KEYWORD, 
                TokenType::TRUE_KEYWORD, 
                TokenType::NUMBER_LITERAL, 
                TokenType::CHARACTER_LITERAL, 
                TokenType::STRING_LITERAL)) {
            return std::make_shared<Literal>(previous());
        }
        
        if (match(TokenType::IDENTIFIER)) {
            return std::make_shared<Variable>(previous());
        }

        if (match(TokenType::LEFT_PAR)) {
            ExprPtr expr = expression();
            consume(TokenType::RIGHT_PAR, "Expect ')' after expression.");
            return std::make_shared<Grouping>(expr);
        }

        throw ParseException(peek(), "Expect expression.");
    }
}