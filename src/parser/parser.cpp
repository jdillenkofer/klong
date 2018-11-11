#include "parser.h"

#include <iostream>
#include <exception>

namespace klong {
    Result<ModulePtr, ParseException> Parser::parse() {
        std::vector<StmtPtr> statements;
        while(!isAtEnd()) {
            statements.push_back(std::move(declarationStmt()));
        }
        Token lastToken = previous();
        std::string moduleName;
        if (lastToken.sourceRange.start.valid()) {
            moduleName = lastToken.sourceRange.start.filename();
        }
        if (!_errors.empty()) {
            return Result<ModulePtr, ParseException>::fromErrors(std::move(_errors));
        }
        auto module = std::make_shared<Module>(moduleName, std::move(statements));
        return Result<ModulePtr, ParseException>::from(std::move(module));
    }

    ParserMemento Parser::saveToMemento() {
        return ParserMemento(_current, _previous, std::move(_lexer->saveToMemento()),
                _isInsideFunction, _isInsideLoop, _isInsideDefer, _errors);
    }

    void Parser::loadFromMemento(ParserMemento& memento) {
        _current = memento._current;
        _previous = memento._previous;
        _lexer->loadFromMemento(memento._lexerMemento);
        _isInsideFunction = memento._isInsideFunction;
        _isInsideLoop = memento._isInsideLoop;
        _isInsideDefer = memento._isInsideDefer;
        _errors = memento._errors;
    }

    Token Parser::consume(TokenType type, std::string errorMessage) {
        if (check(type)) {
            return advance();
        }

        throw ParseException::from(peek(), errorMessage);
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
                case TokenType::RETURN:
                    return;
                default:
                    break;
            }

            advance();
        }
    }

    std::shared_ptr<PrimitiveType> Parser::getPrimitiveTypefromToken(Token token) {
        PrimitiveTypeKind primitiveTypeKind;
        switch (token.type) {                    
            case TokenType::VOID:
                primitiveTypeKind = PrimitiveTypeKind::VOID;
                break;
            case TokenType::BOOL:
                primitiveTypeKind = PrimitiveTypeKind::BOOL;
                break;
            case TokenType::I8:
                primitiveTypeKind = PrimitiveTypeKind::I8;
                break;
            case TokenType::I16:
                primitiveTypeKind = PrimitiveTypeKind::I16;
                break;
            case TokenType::I32:
                primitiveTypeKind = PrimitiveTypeKind::I32;
                break;
            case TokenType::I64:
                primitiveTypeKind = PrimitiveTypeKind::I64;
                break;
            case TokenType::U8:
                primitiveTypeKind = PrimitiveTypeKind::U8;
                break;
            case TokenType::U16:
                primitiveTypeKind = PrimitiveTypeKind::U16;
                break;
            case TokenType::U32:
                primitiveTypeKind = PrimitiveTypeKind::U32;
                break;
            case TokenType::U64:
                primitiveTypeKind = PrimitiveTypeKind::U64;
                break;
            case TokenType::F32:
                primitiveTypeKind = PrimitiveTypeKind::F32;
                break;
            case TokenType::F64:
                primitiveTypeKind = PrimitiveTypeKind::F64;
                break;
            default:
                throw ParseException(token.sourceRange, "Illegal builtin type.");
        }
        return std::make_shared<PrimitiveType>(token.sourceRange, primitiveTypeKind);
    }

    StmtPtr Parser::declarationStmt() {
        try {
            bool isPublic = false;
            Token pubToken;

            if (match(TokenType::PUB)) {
                isPublic = true;
                pubToken = previous();
            }

            if (match(TokenType::FUN)) {
                return function("function", isPublic);
            }

            if (match(TokenType::LET)) {
                return letDeclaration(isPublic);
            }

            if (match(TokenType::CONST)) {
                return constDeclaration(isPublic);
            }

			if (match(TokenType::STRUCT)) {
				return structDeclaration(isPublic);
			}

            if (isPublic) {
                throw ParseException(pubToken.sourceRange, "Illegal pub Keyword.");
            }

            if (match(TokenType::EXTERN)) {
                return externDeclStmt();
            }

            if (match(TokenType::LINE_COMMENT, TokenType::BLOCK_COMMENT)) {
                Token commentToken = previous();
                CommentType type = commentToken.type == TokenType::BLOCK_COMMENT ? CommentType::BLOCK : CommentType::LINE;
                return std::make_shared<Comment>(commentToken.sourceRange, commentToken.value, type);
            }

            return statement();
        } catch(const ParseException& error) {
            _errors.push_back(error);
            synchronize();
            return nullptr;
        }
    }


    std::shared_ptr<ExternalDeclaration> Parser::externDeclStmt() {
        Token ext = previous();
        Token name = consume(TokenType::IDENTIFIER, "Expect extern name.");
        consume(TokenType::COLON, "Expect ':' after extern name.");
        TypePtr type = typeDeclaration();
        Token semicolon = consume(TokenType::SEMICOLON, "Expect ';' after extern declaration.");
        return std::make_shared<ExternalDeclaration>(
                SourceRange { ext.sourceRange.start, semicolon.sourceRange.end }, name.value, type);
    }
    
    std::shared_ptr<Function> Parser::function(const std::string& kind, bool isPublic) {
        auto previousIsInsideFunction = _isInsideFunction;
        _isInsideFunction = true;
        Token fun = previous();
        Token name = consume(TokenType::IDENTIFIER, "Expect " + kind + " name.");
        Token leftPar = consume(TokenType::LEFT_PAR, "Expected '(' after " + kind + " name.");
        std::vector<ParameterPtr> params;
        std::vector<TypePtr> paramTypes;
        if (!check(TokenType::RIGHT_PAR)) {
            do {
                Token identifier = consume(TokenType::IDENTIFIER, "Expect parameter name.");
                consume(TokenType::COLON, "Expect ':' after parameter name.");
                TypePtr type = typeDeclaration();

                params.push_back(
                        std::make_shared<Parameter>(
                                SourceRange { identifier.sourceRange.start, type->sourceRange().end },
                                identifier.value,
                                type));

                if (type->kind() == TypeKind::PRIMITIVE) {
                    auto primitiveType = dynamic_cast<PrimitiveType*>(type.get());
                    if (primitiveType->type() == PrimitiveTypeKind::VOID) {
                        throw ParseException(primitiveType->sourceRange(), "Illegal type 'void' in argument list.");
                    }
                }
                paramTypes.push_back(type);
            } while(match(TokenType::COMMA));
        }
        Token rightPar = consume(TokenType::RIGHT_PAR, "Expect ')' after parameters.");
        TypePtr returnType = nullptr;
		if (match(TokenType::ARROW)) {
            returnType = typeDeclaration();   
        }
        consume(TokenType::LEFT_CURLY_BRACE, "Expect '{' before " + kind + " body.");
        std::vector<StmtPtr> body = blockStmt();
        Token rightCurlyBrace = consume(TokenType::RIGHT_CURLY_BRACE, "Expect '}' after " + kind + " body.");
        auto functionType = std::make_shared<FunctionType>(SourceRange {
            leftPar.sourceRange.start,
            returnType ? returnType->sourceRange().end : rightPar.sourceRange.end
            }, std::move(paramTypes), returnType);

        _isInsideFunction = previousIsInsideFunction;

        return std::make_shared<Function>(SourceRange { fun.sourceRange.start, rightCurlyBrace.sourceRange.end },
                name.value, std::move(params), functionType, std::move(body), isPublic);
    }

    std::vector<StmtPtr> Parser::blockStmt() {
        std::vector<StmtPtr> statements;
        while(!check(TokenType::RIGHT_CURLY_BRACE) && !isAtEnd()) {
            statements.push_back(declarationStmt());
        }
        return statements;
    }

    std::shared_ptr<VariableDeclaration> Parser::letDeclaration(bool isPublic) {
        Token let = previous();
        Token name = consume(TokenType::IDENTIFIER, "Expect variable name.");

        TypePtr type = nullptr;
        if (match(TokenType::COLON)) {
            type = typeDeclaration();
        }
        ExprPtr initializer = nullptr;
        if (match(TokenType::ASSIGN_OP)) {
            initializer = expression();
        }

        if (!_isInsideFunction && dynamic_cast<Literal*>(initializer.get()) == nullptr) {
            throw ParseException(initializer->sourceRange(), "Expect a literal as global let initializer.");
        }
        
        Token semicolon = consume(TokenType::SEMICOLON, "Expect ';' after let declaration.");
        return std::make_shared<VariableDeclaration>(SourceRange { let.sourceRange.start, semicolon.sourceRange.end },
                name.value, type, initializer, isPublic, false, !_isInsideFunction);
    }

    std::shared_ptr<VariableDeclaration> Parser::constDeclaration(bool isPublic) {
        Token constToken = previous();
        Token name = consume(TokenType::IDENTIFIER, "Expect variable name.");

        TypePtr type = nullptr;
        if (match(TokenType::COLON)) {
            type = typeDeclaration();
        }
        consume(TokenType::ASSIGN_OP, "'const' declarations must be initialized.");
        ExprPtr initializer = expression();

        if (!_isInsideFunction && dynamic_cast<Literal*>(initializer.get()) == nullptr) {
            throw ParseException(initializer->sourceRange(), "Expect a literal as global const initializer.");
        }

        Token semicolon = consume(TokenType::SEMICOLON, "Expect ';' after const declaration.");
        return std::make_shared<VariableDeclaration>(SourceRange { constToken.sourceRange.start, semicolon.sourceRange.end },
                name.value, type, initializer, isPublic, true, !_isInsideFunction);
    }
	
	std::shared_ptr<StructDeclaration> Parser::structDeclaration(bool isPublic) {
		Token structToken = previous();
		Token name = consume(TokenType::IDENTIFIER, "Expect struct name.");
		consume(TokenType::LEFT_CURLY_BRACE, "Expect '{' after struct name.");
		std::vector<std::shared_ptr<CustomMember>> members;
		do {
			Token literal = consume(TokenType::IDENTIFIER, "Expect struct element name.");
			Token colon = consume(TokenType::COLON, "Expect colon after element name.");
			auto type = typeDeclaration();
			auto customMember = std::make_shared<CustomMember>(
				SourceRange{ literal.sourceRange.start, type->sourceRange().end },
				literal.value, type);
			members.emplace_back(customMember);
		} while (match(TokenType::COMMA));
		Token rightCurlyBracket = consume(TokenType::RIGHT_CURLY_BRACE, "Expect '}' after last struct member.");
		return std::make_shared<StructDeclaration>(SourceRange{ structToken.sourceRange.start, rightCurlyBracket.sourceRange.end },
			name.value, std::move(members), isPublic);
	}

    TypePtr Parser::typeDeclaration() {
        Token type = peek();
        advance();
        switch(type.type) {
            case TokenType::LEFT_PAR:
            {
                std::vector<TypePtr> argTypes;
                if (peek().type != TokenType::RIGHT_PAR) {
                    do {
                        argTypes.push_back(typeDeclaration());
                    } while(match(TokenType::COMMA));
                }
                consume(TokenType::RIGHT_PAR, "Expect ')' as type list terminator.");
                consume(TokenType::ARROW, "Expect '->' after type list in function type.");
                auto returnType = typeDeclaration();
                return std::make_shared<FunctionType>(
                        SourceRange { type.sourceRange.start, returnType->sourceRange().end },
                        std::move(argTypes), returnType);
            }
            case TokenType::PTR:
            {
                consume(TokenType::LT_OP, "Expect '<' after ptr keyword.");
                auto pointsTo = typeDeclaration();
                auto gtToken = consume(TokenType::GT_OP, "Expect '>' after ptr content.");
                return std::make_shared<PointerType>(
                        SourceRange { type.sourceRange.start, gtToken.sourceRange.end },
                        pointsTo);
            }
            case TokenType::LEFT_SQUARED_BRACKET:
            {
                auto innerType = typeDeclaration();
                consume(TokenType::COMMA, "Expect ',' after array-type declaration.");
                auto arraySizeToken = consume(TokenType::NUMBER_LITERAL, "Expect array size.");
                auto leftSquaredBracket = consume(TokenType::RIGHT_SQUARED_BRACKET,
                                                  "Expect ']' at the end of array-type declaration.");
                uint64_t arraySize;
                arraySizeToken.parse(arraySize);
                auto arrayType = std::make_shared<PointerType>(
                        SourceRange { type.sourceRange.start, leftSquaredBracket.sourceRange.end }, innerType);
                arrayType->isArray(true);
                arrayType->size(arraySize);
                return arrayType;
            }
            case TokenType::VOID:
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
                return getPrimitiveTypefromToken(type);
            }
            case TokenType::IDENTIFIER:
            {
                return std::make_shared<CustomType>(type.sourceRange, type.value);
            }
            default:
                throw ParseException::from(peek(), "Expect type after ':'.");
        }
    }

    std::shared_ptr<If> Parser::ifStmt() {
        Token ifToken = previous();
        consume(TokenType::LEFT_PAR, "Expect '(' after 'if'.");
        ExprPtr condition = expression();
        consume(TokenType::RIGHT_PAR, "Expect ')' after condition.");

        StmtPtr thenBranch = statement();
        StmtPtr elseBranch = nullptr;
        if (match(TokenType::ELSE)) {
            elseBranch = statement();
        }

        return std::make_shared<If>(
                SourceRange { ifToken.sourceRange.start,
                              elseBranch != nullptr ? elseBranch->sourceRange().end : thenBranch->sourceRange().end },
                condition, thenBranch, elseBranch);
    }

    std::shared_ptr<Return> Parser::returnStmt() {
        Token keyword = previous();
        ExprPtr value = nullptr;
        if (!check(TokenType::SEMICOLON)) {
            value = expression();
        }

        Token semicolon = consume(TokenType::SEMICOLON, "Expect ';' after return value.");
        return std::make_shared<Return>(
                SourceRange { keyword.sourceRange.start, semicolon.sourceRange.end },
                value);
    }

    std::shared_ptr<While> Parser::whileStmt() {
        Token whileToken = previous();
        consume(TokenType::LEFT_PAR, "Expect '(' after 'while'.");
        ExprPtr condition = expression();
        consume(TokenType::RIGHT_PAR, "Expect ')' after condition.");
        auto prevIsInsideLoop = _isInsideLoop;
        _isInsideLoop = true;
        StmtPtr body = statement();
        _isInsideLoop = prevIsInsideLoop;
        return std::make_shared<While>(SourceRange { whileToken.sourceRange.start, body->sourceRange().end },
                condition, body);
    }

    std::shared_ptr<For> Parser::forStmt() {
        Token forToken = previous();
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

        auto prevIsInsideLoop = _isInsideLoop;
        _isInsideLoop = true;
        StmtPtr body = statement();
        _isInsideLoop = prevIsInsideLoop;
        
        if (condition == nullptr) {
            // true token
            condition = std::make_shared<BoolLiteral>(true);
        }

        return std::make_shared<For>(SourceRange { forToken.sourceRange.start, body->sourceRange().end },
                initializer, condition, increment, body);
    }

    std::shared_ptr<Break> Parser::breakStmt() {
        Token breakToken = previous();
        auto semicolon = consume(TokenType::SEMICOLON, "Expect ';' after break stmt.");
        return std::make_shared<Break>(SourceRange { breakToken.sourceRange.start, semicolon.sourceRange.end });
    }

    std::shared_ptr<Continue> Parser::continueStmt() {
        Token continueToken = previous();
        auto semicolon = consume(TokenType::SEMICOLON, "Expect ';' after continue stmt.");
        return std::make_shared<Continue>(SourceRange { continueToken.sourceRange.start, semicolon.sourceRange.end });
    }

    std::shared_ptr<Defer> Parser::deferStmt() {
        Token deferToken = previous();
        auto prevIsInsideDefer = _isInsideDefer;
        _isInsideDefer = true;
        auto stmtToDefer = statement();
        _isInsideDefer = prevIsInsideDefer;
        auto semicolon = consume(TokenType::SEMICOLON, "Expect ';' after defer stmt.");
        return std::make_shared<Defer>(
                SourceRange { deferToken.sourceRange.start, semicolon.sourceRange.end }, stmtToDefer);
    }

    std::shared_ptr<Expression> Parser::expressionStmt() {
        ExprPtr expr = expression();
        Token semicolon = consume(TokenType::SEMICOLON, "Expect ';' after expression.");
        return std::make_shared<Expression>(
                SourceRange { expr->sourceRange().start, semicolon.sourceRange.end },
                expr);
    }

    StmtPtr Parser::statement() {
        if (match(TokenType::IF)) {
            return ifStmt();
        }
        if (match(TokenType::RETURN)) {
            if (_isInsideDefer) {
                throw ParseException::from(previous(), "return stmt is not allowed inside defer.");
            }
            return returnStmt();
        }
        if (match(TokenType::WHILE)) {
            return whileStmt();
        }
        if (match(TokenType::FOR)) {
            return forStmt();
        }
        if (match(TokenType::BREAK)) {
            if (!_isInsideLoop) {
                throw ParseException::from(previous(), "break stmt is not allowed outside of loops.");
            }
            if (_isInsideDefer) {
                throw ParseException::from(previous(), "break stmt is not allowed inside defer.");
            }
            return breakStmt();
        }
        if (match(TokenType::CONTINUE)) {
            if (!_isInsideLoop) {
                throw ParseException::from(previous(), "continue stmt is not allowed outside of loops.");
            }
            if (_isInsideDefer) {
                throw ParseException::from(previous(), "continue stmt is not allowed inside defer.");
            }
            return continueStmt();
        }
        if (match(TokenType::DEFER)) {
            if (!_isInsideFunction) {
                throw ParseException::from(previous(), "defer stmt is not allowed outside of functions.");
            }
            if (_isInsideDefer) {
                throw ParseException::from(previous(), "nested defer stmt are not allowed.");
            }
            return deferStmt();
        }
        if (match(TokenType::LEFT_CURLY_BRACE)) {
            Token leftCurlyBrace = previous();
            std::vector<StmtPtr> block = blockStmt();
            Token rightCurlyBrace = consume(TokenType::RIGHT_CURLY_BRACE, "Expect '}' after block.");
            return std::make_shared<Block>(
                    SourceRange { leftCurlyBrace.sourceRange.start, rightCurlyBrace.sourceRange.end },
                    std::move(block));
        }
        return expressionStmt();
    }

    ExprPtr Parser::expression() {
        return assignmentExpr();
    }

    ExprPtr Parser::assignmentExpr() {
        ExprPtr expr = logicalOrExpr();
        if (match(TokenType::ASSIGN_OP)) {
            Token assign = previous();
            ExprPtr value = assignmentExpr();
            if (expr->kind() == ExprKind::VARIABLE) {
                auto variable = std::dynamic_pointer_cast<Variable>(expr);
                return std::make_shared<Assign>(SourceRange { expr->sourceRange().start, variable->sourceRange().end },
                        variable, value);
            }


            if (expr->kind() == ExprKind::UNARY) {
                auto deref = std::dynamic_pointer_cast<Unary>(expr);
                if (deref->op() == UnaryOperation::DEREF) {
                    return std::make_shared<Assign>(SourceRange { expr->sourceRange().start, deref->sourceRange().end },
                                                    deref, value);
                }
            }

			if (expr->kind() == ExprKind::SUBSCRIPT) {
				auto subscript = std::dynamic_pointer_cast<Subscript>(expr);
				return std::make_shared<Assign>(SourceRange{ expr->sourceRange().start, subscript->sourceRange().end },
					subscript, value);
			}

            throw ParseException::from(assign, "Invalid assign target");
        }
        return expr;
    }

    ExprPtr Parser::logicalOrExpr() {
        ExprPtr expr = logicalAndExpr();

        while(match(TokenType::OR)) {
            Token op = previous();
            ExprPtr right = logicalAndExpr();
            expr = std::make_shared<Logical>(
                    SourceRange { expr->sourceRange().start, right->sourceRange().end },
                    expr, LogicalOperation::OR, right);
        }

        return expr;
    }

    ExprPtr Parser::logicalAndExpr() {
        ExprPtr expr = bitwiseOr();

        while(match(TokenType::AND)) {
            Token op = previous();
            ExprPtr right = bitwiseOr();
            expr = std::make_shared<Logical>(
                    SourceRange { expr->sourceRange().start, right->sourceRange().end },
                    expr, LogicalOperation::AND, right);
        }

        return expr;
    }

    ExprPtr Parser::bitwiseOr() {
        ExprPtr expr = bitwiseXOr();

        while(match(TokenType::PIPE)) {
            Token op = previous();
            ExprPtr right = bitwiseXOr();
            expr = std::make_shared<Binary>(
                    SourceRange { expr->sourceRange().start, right->sourceRange().end },
                    expr, BinaryOperation::OR, right);
        }

        return expr;
    }

    ExprPtr Parser::bitwiseXOr() {
        ExprPtr expr = bitwiseAnd();

        while(match(TokenType::CARET)) {
            Token op = previous();
            ExprPtr right = bitwiseAnd();
            expr = std::make_shared<Binary>(
                    SourceRange { expr->sourceRange().start, right->sourceRange().end },
                    expr, BinaryOperation ::XOR, right);
        }

        return expr;
    }

    ExprPtr Parser::bitwiseAnd() {
        ExprPtr expr = equalityExpr();

        while(match(TokenType::AMPERSAND)) {
            Token op = previous();
            ExprPtr right = equalityExpr();
            expr = std::make_shared<Binary>(
                    SourceRange { expr->sourceRange().start, right->sourceRange().end },
                    expr, BinaryOperation::AND, right);
        }

        return expr;
    }

    ExprPtr Parser::equalityExpr() {
        ExprPtr expr = comparisonExpr();

        while(match(TokenType::EQ_OP, TokenType::NE_OP)) {
            Token op = previous();
            BinaryOperation binaryOperation;
            switch (op.type) {
                case TokenType::EQ_OP:
                    binaryOperation = BinaryOperation::EQUALITY;
                    break;
                case TokenType::NE_OP:
                    binaryOperation = BinaryOperation::INEQUALITY;
                    break;
                default:
                    throw ParseException(op.sourceRange, "Expect '==' or '!=' Token.");
            }
            ExprPtr right = comparisonExpr();
            expr = std::make_shared<Binary>(
                    SourceRange{ expr->sourceRange().start, right->sourceRange().end },
                    expr, binaryOperation, right);
        }

        return expr;
    }

    ExprPtr Parser::comparisonExpr() {
        ExprPtr expr = shiftExpr();

        while(match(TokenType::GT_OP, TokenType::GE_OP, TokenType::LT_OP, TokenType::LE_OP)) {
            Token op = previous();
            BinaryOperation binaryOperation;
            switch (op.type) {
                case TokenType::GT_OP:
                    binaryOperation = BinaryOperation::GREATER_THAN;
                    break;
                case TokenType::GE_OP:
                    binaryOperation = BinaryOperation::GREATER_EQUAL;
                    break;
                case TokenType::LT_OP:
                    binaryOperation = BinaryOperation::LESS_THAN;
                    break;
                case TokenType::LE_OP:
                    binaryOperation = BinaryOperation::LESS_EQUAL;
                    break;
                default:
                    throw ParseException(op.sourceRange, "Expect '>', '>=', '<' or '<=' Token.");
            }
            ExprPtr right = shiftExpr();
            expr = std::make_shared<Binary>(
                    SourceRange{ expr->sourceRange().start, right->sourceRange().end },
                    expr, binaryOperation, right);
        }

        return expr;
    }

    ExprPtr Parser::shiftExpr() {
        ExprPtr expr = additionExpr();

        while(match(TokenType::LSL, TokenType::LSR, TokenType::ASR)) {
            Token op = previous();
            BinaryOperation binaryOperation;
            switch (op.type) {
                case TokenType::LSL:
                    binaryOperation = BinaryOperation::LSL;
                    break;
                case TokenType::LSR:
                    binaryOperation = BinaryOperation::LSR;
                    break;
                case TokenType::ASR:
                    binaryOperation = BinaryOperation::ASR;
                    break;
                default:
                    throw ParseException(op.sourceRange, "Expect 'LSL', 'LSR or 'ASR' Token.");
            }
            ExprPtr right = additionExpr();
            expr = std::make_shared<Binary>(
                    SourceRange{ expr->sourceRange().start, right->sourceRange().end },
                    expr, binaryOperation, right);
        }

        return expr;
    }

    ExprPtr Parser::additionExpr() {
        ExprPtr expr = multiplicationExpr();

        while (match(TokenType::MINUS, TokenType::PLUS)) {
            Token op = previous();
            BinaryOperation binaryOperation;
            switch (op.type) {
                case TokenType::MINUS:
                    binaryOperation = BinaryOperation::MINUS;
                    break;
                case TokenType::PLUS:
                    binaryOperation = BinaryOperation::PLUS;
                    break;
                default:
                    throw ParseException(op.sourceRange, "Expect '+' or '-' Token.");
            }
            ExprPtr right = multiplicationExpr();
            expr = std::make_shared<Binary>(
                    SourceRange{ expr->sourceRange().start, right->sourceRange().end },
                    expr, binaryOperation, right);
        }

        return expr;
    }

    ExprPtr Parser::multiplicationExpr() {
        ExprPtr expr = unaryExpr();

        while(match(TokenType::SLASH, TokenType::ASTERISK, TokenType::PERCENT)) {
            Token op = previous();
            BinaryOperation binaryOperation;
            switch (op.type) {
                case TokenType::SLASH:
                    binaryOperation = BinaryOperation::DIVISION;
                    break;
                case TokenType::ASTERISK:
                    binaryOperation = BinaryOperation::MULTIPLICATION;
                    break;
                case TokenType::PERCENT:
                    binaryOperation = BinaryOperation::MODULO;
                    break;
                default:
                    throw ParseException(op.sourceRange, "Expect '*', '/' or '%' Token.");
            }
            ExprPtr right = unaryExpr();
            expr = std::make_shared<Binary>(
                    SourceRange{ expr->sourceRange().start, right->sourceRange().end },
                    expr, binaryOperation, right);
        }

        return expr;
    }

    ExprPtr Parser::unaryExpr() {
        if (match(TokenType::BANG)) {
            Token op = previous();
            ExprPtr right = unaryExpr();
            return std::make_shared<Unary>(
                    SourceRange { op.sourceRange.start, right->sourceRange().end },
                    UnaryOperation::NOT, right);
        }

        if (match(TokenType::MINUS)) {
            Token op = previous();
            ExprPtr right = unaryExpr();
            return std::make_shared<Unary>(
                    SourceRange { op.sourceRange.start, right->sourceRange().end },
                    UnaryOperation::MINUS, right);
        }

        if (match(TokenType::AMPERSAND)) {
            Token op = previous();
            ExprPtr right = unaryExpr();
            return std::make_shared<Unary>(
                    SourceRange { op.sourceRange.start, right->sourceRange().end },
                    UnaryOperation::ADDRESS_OF, right);
        }

        if (match(TokenType::ASTERISK)) {
            Token op = previous();
            ExprPtr right = unaryExpr();
            return std::make_shared<Unary>(
                    SourceRange { op.sourceRange.start, right->sourceRange().end },
                    UnaryOperation::DEREF, right);
        }

        if (match(TokenType::SIZEOF)) {
            Token sizeOf = previous();
            consume(TokenType::LEFT_PAR, "Expect '(' after sizeof operator.");
            TypePtr right = typeDeclaration();
            Token rightPar = consume(TokenType::RIGHT_PAR, "Expect ')' at the end of size_of operator.");
            return std::make_shared<SizeOf>(
                    SourceRange { sizeOf.sourceRange.start, rightPar.sourceRange.end },
                    right);
        }

        if (match(TokenType::CAST)) {
            Token cast = previous();
            consume(TokenType::LT_OP, "Expect '<' after cast operator.");
            TypePtr targetType = typeDeclaration();
            consume(TokenType::GT_OP, "Expect '>' after target type.");
            consume(TokenType::LEFT_PAR, "Expect '(' after target type.");
            ExprPtr right = assignmentExpr();
            Token rightPar = consume(TokenType::RIGHT_PAR, "Expect ')' after cast target.");
            return std::make_shared<Cast>(
                    SourceRange { cast.sourceRange.start, rightPar.sourceRange.end },
                    targetType, right);
        }

        return postfixExpr();
    }

    ExprPtr Parser::postfixExpr() {
        ExprPtr primaryExpr = primary();
		
		auto memento = saveToMemento();
		ExprPtr prevLoopExpr = primaryExpr;
		while(true) {
			auto loopExpr = finishPostfixExpr(prevLoopExpr);
			if (!loopExpr) {
				if (!prevLoopExpr) {
					break;
				} else {
					return prevLoopExpr;
				}
			}
			prevLoopExpr = loopExpr;
        }
		loadFromMemento(memento);

        return primaryExpr;
    }

	ExprPtr Parser::finishPostfixExpr(ExprPtr lhs) {
		if (match(TokenType::LEFT_PAR)) {
			auto callExpr = finishCallExpr(lhs);
			return callExpr;
		}

		if (match(TokenType::LEFT_SQUARED_BRACKET)) {
			auto subscriptExpr = finishSubscriptExpr(lhs);
			return subscriptExpr;
		}

		return nullptr;
	}

	ExprPtr Parser::finishCallExpr(ExprPtr callee) {
		Token leftPar = previous();
		std::vector<ExprPtr> args;
		if (!check(TokenType::RIGHT_PAR)) {
			do {
				args.push_back(expression());
			} while (match(TokenType::COMMA));
		}
		Token rightPar = consume(TokenType::RIGHT_PAR, "Expect ')' after arguments.");

		return std::make_shared<Call>(
			SourceRange{ leftPar.sourceRange.start, rightPar.sourceRange.end },
			callee, std::move(args));
	}

	ExprPtr Parser::finishSubscriptExpr(ExprPtr target)
	{
		Token leftPar = previous();
		auto index = expression();
		Token rightPar = consume(TokenType::RIGHT_SQUARED_BRACKET, "Expect ']' after subscript operator.");

		return std::make_shared<Subscript>(
			SourceRange{ leftPar.sourceRange.start, rightPar.sourceRange.end },
			target, index);
	}

    ExprPtr Parser::primary() {
		if (match(TokenType::IDENTIFIER)) {
			Token identifier = previous();
			auto variableExpr = std::make_shared<Variable>(identifier.sourceRange, identifier.value);
			return variableExpr;
		}

		if (match(TokenType::LEFT_PAR)) {
			Token leftPar = previous();
			ExprPtr expr = expression();
			Token rightPar = consume(TokenType::RIGHT_PAR, "Expect ')' after expression.");
			return std::make_shared<Grouping>(
				SourceRange{ leftPar.sourceRange.start, rightPar.sourceRange.end },
				expr);
		}

		return literal();
    }

    ExprPtr Parser::literal() {
        Token literalToken = peek();
        if (match(TokenType::FALSE_KEYWORD,
                  TokenType::TRUE_KEYWORD)) {
            return std::make_shared<BoolLiteral>(literalToken.sourceRange, literalToken.type == TokenType::TRUE_KEYWORD);
        }

        if (match(TokenType::NUMBER_LITERAL)) {
            // TODO: enhance conversion result error handling
            switch (literalToken.numberType) {
                case NumberType::INT:
                {
                    int64_t i;
                    auto conversionResult = literalToken.parse(i);
                    if (conversionResult == NumberConversionResult::OK) {
                        return std::make_shared<NumberLiteral>(literalToken.sourceRange, i);
                    }
                    break;
                }
                case NumberType::UINT:
                {
                    uint64_t u;
                    auto conversionResult = literalToken.parse(u);
                    if (conversionResult == NumberConversionResult::OK) {
                        return std::make_shared<NumberLiteral>(literalToken.sourceRange, u);
                    }
                    break;
                }
                case NumberType::FLOAT:
                {
                    double d;
                    auto conversionResult = literalToken.parse(d);
                    if (conversionResult == NumberConversionResult::OK) {
                        return std::make_shared<NumberLiteral>(literalToken.sourceRange, d);
                    }
                    break;
                }
                case NumberType::NONE:
                default:
                    throw ParseException(literalToken.sourceRange, "Unexpected numberType.");
            }
            throw ParseException(literalToken.sourceRange, "Couldn't convert numberLiteral.");
        }

        if (match(TokenType::CHARACTER_LITERAL)) {
            char c = literalToken.value[0];
            return std::make_shared<CharacterLiteral>(literalToken.sourceRange, c);
        }

        if (match(TokenType::STRING_LITERAL)) {
            return std::make_shared<StringLiteral>(literalToken.sourceRange, literalToken.value);
        }

        if (match(TokenType::LEFT_SQUARED_BRACKET)) {
            std::vector<ExprPtr> values;
            do {
                values.push_back(literal());
            } while(match(TokenType::COMMA));
            auto rightSquaredBracket = consume(TokenType::RIGHT_SQUARED_BRACKET, "Expect ']' at the end of an array literal.");
            return std::make_shared<ArrayLiteral>(
                    SourceRange { literalToken.sourceRange.start, rightSquaredBracket.sourceRange.end }, std::move(values));
        }
		throw ParseException(literalToken.sourceRange, "Couldn't parse literal.");
    }
}