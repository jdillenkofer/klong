#include <cassert>
#include <functional>

#include "type_check_visitor.h"

#include "ast/module.h"
#include "ast/stmt.h"
#include "ast/expr.h"
#include "ast/type.h"
#include "typechecker.h"

namespace klong {

    void TypeCheckVisitor::check(const std::vector<Stmt*>& statements) {
        for (const auto& stmt : statements) {
            check(stmt);
        }
    }

    void TypeCheckVisitor::check(Stmt* stmt) {
        if (stmt != nullptr) {
            stmt->accept(this);
        }
    }

    void TypeCheckVisitor::check(Expr* expr) {
        if (expr != nullptr) {
            expr->accept(this);
            resolveType(expr->type());
        }
    }

    void TypeCheckVisitor::resolveType(Type* type) {
        if (type) {
            type->accept(this);
        }
    }

    void TypeCheckVisitor::declareType(TypeDeclaration* typeDeclarationStmt) {
        if (_typeDeclarations.find(typeDeclarationStmt->name()) != _typeDeclarations.end()) {
            _session->reportError("Type '" + typeDeclarationStmt->name() + "' already declared.", typeDeclarationStmt->sourceRange());
        }
        if (typeDeclarationStmt->isPublic()) {
            auto isAlreadyDeclared = !_session->declareType(typeDeclarationStmt->name(), typeDeclarationStmt);
            if (isAlreadyDeclared) {
                _session->reportError("Type '" + typeDeclarationStmt->name() + "' already declared.", typeDeclarationStmt->sourceRange());
            }
        }
        _typeDeclarations[typeDeclarationStmt->name()] = typeDeclarationStmt;
    }

    bool TypeCheckVisitor::getAndResetReturnsValue() {
        auto returnsValue = _returnsValue;
        _returnsValue = false;
        return returnsValue;
    }

    TypePtr TypeCheckVisitor::applyIntegerPromotion(Type* type) {
        if (type->kind() == TypeKind::PRIMITIVE) {
            auto primitiveType = static_cast<PrimitiveType*>(type);
            if (primitiveType->isInteger()) {
                if (primitiveType->isSigned()) {
                    return std::make_shared<PrimitiveType>(type->sourceRange(), PrimitiveTypeKind::I64);
                }
                else {
                    return std::make_shared<PrimitiveType>(type->sourceRange(), PrimitiveTypeKind::U64);
                }
            }
            if (primitiveType->isFloat()) {
                return std::make_shared<PrimitiveType>(type->sourceRange(), PrimitiveTypeKind::F64);
            }
        }
        return nullptr;
    }

    TypePtr TypeCheckVisitor::applyArithmeticPromotion(Type* left, Type* right) {
        size_t leftIndex = 0;
        size_t rightIndex = 0;
        for (auto& type : _arithmeticConversionStack) {
            if (type->isEqual(left)) {
                break;
            }
            leftIndex++;
        }

        for (auto& type : _arithmeticConversionStack) {
            if (type->isEqual(right)) {
                break;
            }
            rightIndex++;
        }
        if (leftIndex > rightIndex) {
            return _arithmeticConversionStack[leftIndex];
        } else {
            return _arithmeticConversionStack[rightIndex];
        }
    }

    // Module
    void TypeCheckVisitor::visitModule(Module* module) {
        for (const auto& statement : module->statements()) {
            if (statement->kind() == StatementKind::TYPE_DECL) {
                auto stmt = static_cast<TypeDeclaration*>(statement);
                declareType(stmt);
            }
        }
        for (auto& dependency : module->dependencies()) {
            if (!_session->isTypechecked(dependency->absolutepath())) {
                auto typechecker = std::make_shared<TypeChecker>();
                typechecker->check(dependency, _session);
                _session->completeTypechecked(dependency->absolutepath());
            }
        }
        check(module->statements());
    }

    // Stmt
    void TypeCheckVisitor::visitBlockStmt(Block* stmt) {
        check(stmt->statements());
    }

    void TypeCheckVisitor::visitExpressionStmt(Expression* stmt) {
        check(stmt->expression());
    }

    void TypeCheckVisitor::visitExtDeclStmt(ExternalDeclaration* stmt) {
        resolveType(stmt->type());
    }

    void TypeCheckVisitor::visitImportStmt(Import* stmt) {
        // nothing to do here
        (void) stmt;
    }

    void TypeCheckVisitor::visitFunctionStmt(Function* stmt) {
        resolveType(stmt->functionType());
        auto previousFunction = currentFunction;
        currentFunction = stmt;
        for (auto& param : stmt->params()) {
            check(param);
        }
        _returnsValue = false;
        check(stmt->body());
        auto primType = static_cast<PrimitiveType*>(stmt->functionType()->returnType());
        if (!_returnsValue && primType != nullptr && !primType->isVoid()) {
            _session->reportError("Control-flow reaches end of non-void function " + stmt->name() + ".", stmt->sourceRange());
        }
        currentFunction = previousFunction;
    }

    void TypeCheckVisitor::visitParameterStmt(Parameter* stmt) {
        if (stmt->type()->kind() == TypeKind::FUNCTION) {
            _session->reportError("Parameters of type functionType are not allowed.", stmt->sourceRange());
        }
        if (stmt->type()->kind() == TypeKind::POINTER) {
            auto pointerType = static_cast<PointerType*>(stmt->type());
            if (pointerType->isArray()) {
                _session->reportError("Parameters of type array are not allowed.", stmt->sourceRange());
            }
        }
    }

    void TypeCheckVisitor::visitIfStmt(If* stmt) {
        check(stmt->condition());
        if (!Type::isBoolean(stmt->condition()->type())) {
            _session->reportError("Expect bool condition in if-statement.", stmt->condition()->sourceRange());
        }
        check(stmt->thenBranch());
        bool thenBranchReturnsValue = getAndResetReturnsValue();
        check(stmt->elseBranch());
        if (stmt->elseBranch() != nullptr) {
            _returnsValue = getAndResetReturnsValue() && thenBranchReturnsValue;
        } else {
            _returnsValue = false;
        }
        if (_returnsValue) {
			stmt->setMergeUnreachable();
        }
    }

    void TypeCheckVisitor::visitReturnStmt(Return* stmt) {
        check(stmt->value());
        if (stmt->value() != nullptr) {
            auto functionReturnType = currentFunction->functionType()->returnType();
            auto valueType = stmt->value()->type();
            if (!functionReturnType->isEqual(valueType)) {
                if (!Type::isVoidPtrCast(stmt->value(), functionReturnType)) {
                    _session->reportError("Expect return statement type to match the function returnType.", stmt->sourceRange());
                }
            }
            _returnsValue = true;
        }
    }

    void TypeCheckVisitor::visitVarDeclStmt(VariableDeclaration* stmt) {
        if (currentFunction != nullptr && stmt->isPublic()) {
            _session->reportError("Pub keyword not allowed in front of local variable.", stmt->sourceRange());
        }
        check(stmt->initializer());

        if (stmt->type() == nullptr) {
            auto initializerType = stmt->initializer()->type();
            if (initializerType) {
                auto clonedInitType = std::shared_ptr<Type>(initializerType->clone());

                auto initAsPointerType = dynamic_cast<PointerType*>(initializerType);

                // propagate array type meta info
                if (initAsPointerType && initAsPointerType->isArray()) {
                    auto clonedInitasPointerType = dynamic_cast<PointerType*>(clonedInitType.get());
                    clonedInitasPointerType->isArray(true);
                    clonedInitasPointerType->size(initAsPointerType->size());
                }

                stmt->type(clonedInitType);
            }
        } else {
            resolveType(stmt->type());
            auto stmtAsPointerType = dynamic_cast<PointerType*>(stmt->type());
            if (stmtAsPointerType && stmtAsPointerType->isArray()
                && stmt->initializer() && stmt->initializer()->kind() != ExprKind::LITERAL) {
                if (!Type::isVoidPtrCast(stmt->initializer(), stmt->type())) {
                    _session->reportError("initializer of arrays can only contain array literals.", stmt->sourceRange());
                }
            }
            if (stmt->initializer() && !stmt->type()->isEqual(stmt->initializer()->type())) {
                if (!Type::isVoidPtrCast(stmt->initializer(), stmt->type())) {
                    _session->reportError("initializerType doesn't match declaration type.", stmt->sourceRange());
                }
            }
        }
    }

	/**
	 * this is the shared typecheck implementation for structs and unions
	 */
	void TypeCheckVisitor::checkMemberTypeDeclStmt(MemberTypeDeclaration* stmt) {
		for (auto& value : stmt->members()) {
			check(value);

            bool isCustomType = value->type()->kind() == TypeKind::CUSTOM;
            if (isCustomType) {
                auto customType = static_cast<CustomType*>(value->type());
                if (customType && customType->name() == stmt->name()) {
                    _session->reportError("Self referential member definitions are not allowed.", value->sourceRange());
                }
            }
		}
	}

	void TypeCheckVisitor::visitStructDeclStmt(StructDeclaration* stmt) {
		checkMemberTypeDeclStmt(stmt);
	}

    void TypeCheckVisitor::visitUnionDeclStmt(UnionDeclaration* stmt) {
		checkMemberTypeDeclStmt(stmt);
    }

	void TypeCheckVisitor::visitEnumDeclStmt(EnumDeclaration* stmt) {
		(void) stmt;
	}

	void TypeCheckVisitor::visitCustomMemberStmt(CustomMember* stmt) {
        // nothing to do here
        resolveType(stmt->type());
	}

    void TypeCheckVisitor::visitWhileStmt(While* stmt) {
        check(stmt->condition());
        if (!Type::isBoolean(stmt->condition()->type())) {
            _session->reportError("while condition expects bool type.", stmt->condition()->sourceRange());
        }
        check(stmt->body());
    }

    void TypeCheckVisitor::visitForStmt(For* stmt) {
        check(stmt->initializer());
        check(stmt->condition());
        if (!Type::isBoolean(stmt->condition()->type())) {
            _session->reportError("for condition expects bool type.", stmt->condition()->sourceRange());
        }
        check(stmt->increment());
        check(stmt->body());
    }


    void TypeCheckVisitor::visitBreakStmt(Break* stmt) {
        // empty on purpose
        (void) stmt;
    }

    void TypeCheckVisitor::visitContinueStmt(Continue* stmt) {
        // empty on purpose
        (void) stmt;
    }

    void TypeCheckVisitor::visitDeferStmt(Defer* stmt) {
        check(stmt->stmtToDefer());
    }

    void TypeCheckVisitor::visitCommentStmt(Comment* stmt) {
        // empty on purpose
        (void) stmt;
    }

    // Expr
    void TypeCheckVisitor::visitAssignExpr(Assign* expr) {
        check(expr->target());
        check(expr->value());

        Type* targetType = expr->target()->type();

        if (!targetType || !expr->value()->type()) {
            return;
        }

        if (!targetType->isEqual(expr->value()->type())) {
            if (!Type::isVoidPtrCast(expr->value(), targetType)) {
                _session->reportError("Expect valid type in assignment.", expr->value()->sourceRange());
            }
        }

        if (expr->value()->type()) {
            expr->type(std::shared_ptr<Type>(expr->value()->type()->clone()));
        }
    }

    void TypeCheckVisitor::visitBinaryExpr(Binary* expr) {
        check(expr->left());
        check(expr->right());
        
		auto leftType = expr->left()->type();
        auto rightType = expr->right()->type();
        if (!leftType) {
            return;
        }
        auto resultType = std::shared_ptr<Type>(leftType->clone());
		
		auto applyBinaryPromotion = [this](Type*& lhsType, Type*& rhsType, TypePtr& resType) {
			if (!lhsType->isEqual(rhsType)) {
				auto promotedLeftType = applyIntegerPromotion(lhsType);
				auto promotedRightType = applyIntegerPromotion(rhsType);
				if (promotedLeftType && promotedRightType &&
					promotedLeftType->isEqual(promotedRightType.get())) {
					resType = promotedLeftType;
				}
				else {
					// arithmetic promotion
					auto arithmeticPromotedType = applyArithmeticPromotion(lhsType, rhsType);
					resType = arithmeticPromotedType;
				}
			}
		};

        switch(expr->op())
        {
            case BinaryOperation::PLUS:
            case BinaryOperation::MINUS:
            {
                if (Type::isPointer(leftType) && Type::isInteger(rightType)) {
                    expr->type(std::shared_ptr<Type>(leftType->clone()));
                    break;
                }
                [[fallthrough]];
            }
            case BinaryOperation::MULTIPLICATION:
            case BinaryOperation::DIVISION:
            {
                if ((Type::isInteger(leftType) || Type::isFloat(leftType))
                    && (Type::isInteger(rightType) || Type::isFloat(rightType))) {
					applyBinaryPromotion(leftType, rightType, resultType);
                    expr->castToType(resultType);
                    expr->type(resultType);
                    break;
                }
				_session->reportError("Illegal type in arithmetic operation.", expr->sourceRange());
				break;
            }
			case BinaryOperation::EQUALITY:
			case BinaryOperation::INEQUALITY:
			{
				if (Type::isCustom(leftType) && Type::isCustom(rightType)
					&& static_cast<CustomType*>(leftType)->resolvesTo()->typeDeclarationKind() == TypeDeclarationKind::ENUM
					&& leftType->isEqual(rightType)) {
					expr->type(std::make_shared<PrimitiveType>(PrimitiveTypeKind::BOOL));
					break;
				}
			}
			[[fallthrough]];
            case BinaryOperation::GREATER_THAN:
            case BinaryOperation::GREATER_EQUAL:
            case BinaryOperation::LESS_THAN:
            case BinaryOperation::LESS_EQUAL:
            {
                if ((Type::isInteger(leftType) || Type::isFloat(leftType)) 
					&& (Type::isInteger(rightType) || Type::isFloat(rightType))) {
					applyBinaryPromotion(leftType, rightType, resultType);
                    expr->castToType(resultType);
                    expr->type(std::make_shared<PrimitiveType>(PrimitiveTypeKind::BOOL));
                    break;
                }
                if (Type::isPointer(leftType) && Type::isPointer(rightType)) {
                    auto leftPointerType = static_cast<PointerType*>(leftType);
                    auto rightPointerType = static_cast<PointerType*>(rightType);
                    if (leftType->isEqual(rightType) || Type::isVoid(leftPointerType->pointsTo()) || Type::isVoid(rightPointerType->pointsTo())) {
                        expr->type(std::make_shared<PrimitiveType>(PrimitiveTypeKind::BOOL));
                    }
                    break;
                }

                _session->reportError("Comparisons must be of the same type.", expr->sourceRange());
                break;
            }
            case BinaryOperation::MODULO:
            case BinaryOperation::LSL:
            case BinaryOperation::LSR:
            case BinaryOperation::ASR:
            case BinaryOperation::AND:
            case BinaryOperation::XOR:
            case BinaryOperation::OR:
            {
                if (Type::isInteger(leftType) && Type::isInteger(rightType) && leftType->isEqual(rightType)) { // @Robustness: does the types really have to match here?
                    expr->type(resultType);
                    break;
                }
                _session->reportError("Illegal type in binary operation", expr->sourceRange());
                break;
            }
            default:
                _session->reportError("Illegal binary op.", expr->sourceRange());
                break;
        }
    }

    void TypeCheckVisitor::visitCallExpr(Call* expr) {
        check(expr->callee());
        std::vector<Type*> callParamTypes;
        for (const auto& arg : expr->args()) {
            check(arg);
        }
        auto calleeType = expr->callee()->type();
        if (calleeType->kind() == TypeKind::POINTER) {
            auto calleePointer = static_cast<PointerType*>(calleeType);
            auto pointsToFunction = calleePointer->pointsTo()->kind() == TypeKind::FUNCTION;
            if (pointsToFunction) {
                auto functionType = static_cast<FunctionType*>(calleePointer->pointsTo());
                auto arguments = expr->args();
                if (!functionType->matchesSignature(arguments)) {
                    _session->reportError("Call Expr doesn't match function signature.", expr->sourceRange());
                }
                expr->type(std::shared_ptr<Type>(functionType->returnType()->clone()));
                return;
            }
        }
        _session->reportError("Callee doesn't resolve to function pointer expression.", expr->sourceRange());
    }

    void TypeCheckVisitor::visitGroupingExpr(Grouping* expr) {
        check(expr->expression());
        expr->type(std::shared_ptr<Type>(expr->expression()->type()->clone()));
    }

	void TypeCheckVisitor::visitSubscriptExpr(Subscript* expr) {
		check(expr->target());
        if (expr->target()->type()->kind() != TypeKind::POINTER) {
            _session->reportError("Illegal target type for subscript expr. Target has to be of type pointer.", expr->sourceRange());
            return;
        }
		check(expr->index());
        if (expr->index()->type()->kind() != TypeKind::PRIMITIVE) {
            _session->reportError("Index of subscript operator has to be numeric.", expr->sourceRange());
            return;
        }
		auto pointerType = static_cast<PointerType*>(expr->target()->type());
		auto numberType = static_cast<PrimitiveType*>(expr->index()->type());
		if (!numberType->isInteger()) {
			_session->reportError("Index of subscript operator has to be numeric.", expr->sourceRange());
		}
		auto innerType = std::shared_ptr<Type>(pointerType->pointsTo()->clone());
		expr->type(innerType);
	}

	void TypeCheckVisitor::visitMemberAccessExpr(MemberAccess* expr) {
        check(expr->target());
        auto customType = dynamic_cast<CustomType*>(expr->target()->type());
        auto pointerType = dynamic_cast<PointerType*>(expr->target()->type());
        if (pointerType) {
            customType = dynamic_cast<CustomType*>(pointerType->pointsTo());
        }

        if (!customType) {
            _session->reportError("MemberAccess target is not a custom type.", expr->sourceRange());
            return;
        }

        auto declarationType = customType->resolvesTo();
		if (declarationType == nullptr) {
			return;
		}
        switch(declarationType->typeDeclarationKind()) {
            case TypeDeclarationKind::STRUCT: 
			case TypeDeclarationKind::UNION: {
                auto memberTypeDecl = static_cast<MemberTypeDeclaration*>(declarationType);
                auto memberPtr = memberTypeDecl->findMember(expr->value());
                if (!memberPtr) {
                    _session->reportError("MemberAccess target does not have such a member element.", expr->sourceRange());
                } else {
                    expr->type(std::shared_ptr<Type>(memberPtr->type()->clone()));
                }
                break;
            }
			case TypeDeclarationKind::ENUM: {
				_session->reportError("MemberAccess target does not allow enum types. Try to use the '::' operator instead.", expr->sourceRange());
				break;
			}
            default:
                assert(false);
        }
    }

	void TypeCheckVisitor::visitEnumAccessExpr(EnumAccess* expr) {
		resolveType(expr->target());
		auto resolvedTypeDecl = expr->target()->resolvesTo();
		if (!resolvedTypeDecl || resolvedTypeDecl->typeDeclarationKind() != TypeDeclarationKind::ENUM) {
			_session->reportError("Expect enum type.", expr->target()->sourceRange());
		} else {
			auto enumValues = static_cast<EnumDeclaration*>(resolvedTypeDecl)->values();
			auto it = std::find(enumValues.begin(), enumValues.end(), expr->value());
			if (it == enumValues.end()) {
				_session->reportError("No such value " + expr->value() + " in enum type " + expr->target()->name() + ".", expr->sourceRange());
			}
			expr->type(std::shared_ptr<Type>(expr->target()->clone()));
		}
	}

    void TypeCheckVisitor::visitLogicalExpr(Logical* expr) {
        check(expr->left());
        auto leftType = expr->left()->type();
        if (!leftType) {
            return;
        }
        if (!Type::isBoolean(leftType)) {
            _session->reportError("Expect boolean expr.", expr->left()->sourceRange());
        }
        check(expr->right());
        auto rightType = expr->right()->type();
        if (!rightType) {
            return;
        }
        if (!Type::isBoolean(rightType)) {
            _session->reportError("Expect boolean expr.", expr->right()->sourceRange());
        }
        expr->type(std::make_shared<PrimitiveType>(PrimitiveTypeKind::BOOL));
    }

    void TypeCheckVisitor::visitUnaryExpr(Unary* expr) {
        check(expr->right());

        auto rightType = expr->right()->type();
        if (!rightType) {
            return;
        }

        if (expr->op() == UnaryOperation::NOT && !Type::isBoolean(rightType)) {
            _session->reportError("'!' expects boolean expression.", expr->sourceRange());
        }

        if (expr->op() == UnaryOperation::MINUS && !Type::isInteger(rightType)) {
            _session->reportError("Unary '-' expects number expression.", expr->sourceRange());
        }

        if (expr->op() == UnaryOperation::DEREF) {
            if (!Type::isPointer(rightType)) {
                _session->reportError("Deref expects pointer type.", expr->sourceRange());
                return;
            }
            auto pointerType = static_cast<PointerType*>(rightType);
            if (pointerType->pointsTo()->kind() == TypeKind::FUNCTION) {
                _session->reportError("Deref expects non function pointer type.", expr->sourceRange());
            }
            expr->type(std::shared_ptr<Type>(pointerType->pointsTo()->clone()));
            return;
        }

        if (expr->op() == UnaryOperation::ADDRESS_OF) {
            auto variable = dynamic_cast<Variable*>(expr->right());
            if (variable == nullptr) {
                _session->reportError("Can only get address of variable expressions.", expr->sourceRange());
            } else {
                auto isFunction = false;
                if (variable->type()->kind() == TypeKind::FUNCTION) {
                    isFunction = true;
                }

                if (variable->type()->kind() == TypeKind::POINTER) {
                    auto pointerType = static_cast<PointerType*>(variable->type());
                    if (pointerType->pointsTo()->kind() == TypeKind::FUNCTION) {
                        isFunction = true;
                    }
                }

                if (isFunction) {
                    _session->reportError("Can not get address of function. Function names are already pointers.", expr->sourceRange());
                }
            }
            expr->type(std::make_shared<PointerType>(
                    expr->sourceRange(),
                    std::shared_ptr<Type>(rightType->clone())));
            return;
        }
        expr->type(std::shared_ptr<Type>(rightType->clone()));
    }

    void TypeCheckVisitor::visitSizeOfExpr(SizeOf *expr) {
        resolveType(expr->right());
        if (expr->right()->kind() == TypeKind::FUNCTION) {
            _session->reportError("Can not get sizeof a function type. Did you mean sizeof<ptr type>?", expr->right()->sourceRange());
        }
        expr->type(std::make_shared<PrimitiveType>(expr->sourceRange(), PrimitiveTypeKind::U64));
    }

    void TypeCheckVisitor::visitCastExpr(Cast* expr) {
        check(expr->right());
        auto sourceType = expr->right()->type();
        if (!sourceType) {
            return;
        }
        if (Type::isVoid(sourceType)) {
            _session->reportError("Can not cast from void type.", sourceType->sourceRange());
        }
        if (sourceType->kind() == TypeKind::CUSTOM) {
            _session->reportError("Can not cast from custom type.", expr->targetType()->sourceRange());
        }
        auto targetType = std::shared_ptr<Type>(expr->targetType()->clone());
        if (expr->targetType()->kind() == TypeKind::FUNCTION) {
            _session->reportError("Can not cast to function type. Did you mean ptr to function?", expr->targetType()->sourceRange());
        }
        if (expr->targetType()->kind() == TypeKind::CUSTOM) {
            _session->reportError("Can not cast to custom type. Did you mean ptr to custom type?", expr->targetType()->sourceRange());
        }
        expr->type(targetType);
    }

    void TypeCheckVisitor::visitVariableExpr(Variable* expr) {
        Stmt* resolvesTo = expr->resolvesTo();
        switch(resolvesTo->kind()) {
            case StatementKind::FUNCTION:
            {
                auto function = static_cast<Function*>(resolvesTo);
                auto clonedFunction = std::shared_ptr<Type>(function->functionType()->clone());
                auto pointerToClonedFunction = std::make_shared<PointerType>(clonedFunction->sourceRange(), clonedFunction);
                expr->type(pointerToClonedFunction);
                break;
            }
            case StatementKind::EXT_DECL:
            {
                auto extDecl = static_cast<ExternalDeclaration*>(resolvesTo);
                auto clonedType = std::shared_ptr<Type>(extDecl->type()->clone());
                if (clonedType->kind() == TypeKind::FUNCTION) {
                    clonedType = std::make_shared<PointerType>(clonedType->sourceRange(), clonedType);
                }
                expr->type(clonedType);
                break;
            }
            case StatementKind::VAR_DECL:
            {
                auto varDecl = static_cast<VariableDeclaration*>(resolvesTo);
                auto clonedVarDeclType = std::shared_ptr<Type>(varDecl->type()->clone());
                // propagate array type meta info
                if (varDecl->type()->kind() == TypeKind::POINTER) {
                    auto varDeclAsPointerType = static_cast<PointerType*>(varDecl->type());
                    if (varDeclAsPointerType->isArray()) {
                        auto clonedVarDeclTypeAsPtr = static_cast<PointerType*>(clonedVarDeclType.get());
                        clonedVarDeclTypeAsPtr->isArray(true);
                        clonedVarDeclTypeAsPtr->size(varDeclAsPointerType->size());
                    }
                }
                expr->type(clonedVarDeclType);
                break;
            }
            case StatementKind::PARAMETER:
            {
                auto paramStmt = static_cast<Parameter*>(resolvesTo);
                expr->type(std::shared_ptr<Type>(paramStmt->type()->clone()));
                break;
            }
            default:
                _session->reportError("Variable resolves to invalid kind.", expr->sourceRange());
        }
    }

    // Literals
    void TypeCheckVisitor::visitNumberLiteral(NumberLiteral* expr) {
        auto type = std::shared_ptr<Type>(expr->literalType()->clone());
        expr->type(type);
    }

    void TypeCheckVisitor::visitBoolLiteral(BoolLiteral* expr) {
        auto type = std::make_shared<PrimitiveType>(expr->sourceRange(), PrimitiveTypeKind::BOOL);
        expr->type(type);
    }

    void TypeCheckVisitor::visitNullLiteral(NullLiteral* expr) {
        auto voidType = std::make_shared<PrimitiveType>(PrimitiveTypeKind::VOID);
        auto voidPtrType = std::make_shared<PointerType>(expr->sourceRange(), voidType);
        expr->type(voidPtrType);
    }

    void TypeCheckVisitor::visitStringLiteral(StringLiteral* expr) {
        auto type = std::make_shared<PointerType>(expr->sourceRange(),
                std::make_shared<PrimitiveType>(expr->sourceRange(), PrimitiveTypeKind::I8));
        expr->type(type);
    }

    void TypeCheckVisitor::visitCharacterLiteral(CharacterLiteral* expr) {
        auto type = std::make_shared<PrimitiveType>(expr->sourceRange(), PrimitiveTypeKind::I8);
        expr->type(type);
    }

    void TypeCheckVisitor::visitArrayLiteral(ArrayLiteral* expr) {
        for (auto& val : expr->values()) {
            check(val);
        }

        auto valueType = expr->values()[0]->type();
        for (auto& val : expr->values()) {
            if (!valueType->isEqual(val->type())) {
                throw CompilationIncident(CompilationIncidentType::ERROR, "Not all array values are of the same type.", expr->sourceRange());
            }
        }

        auto type = std::make_shared<PointerType>(expr->sourceRange(),
                std::shared_ptr<Type>(valueType->clone()));
        type->isArray(true);
        type->size(expr->values().size());
        expr->type(type);
    }

    void TypeCheckVisitor::visitFunctionType(FunctionType* type) {
        for (auto& param : type->paramTypes()) {
            resolveType(param);
            if (param->kind() == TypeKind::FUNCTION) {
                _session->reportError("Parameters of type functionType are not allowed.", param->sourceRange());
            }
            if (param->kind() == TypeKind::PRIMITIVE) {
                if (Type::isVoid(param)) {
                    _session->reportError("Parameters of type void are not allowed.", param->sourceRange());
                }
            }
        }

        resolveType(type->returnType());
    }

    void TypeCheckVisitor::visitPrimitiveType(PrimitiveType *type) {
        // nothing to do here
        (void) type;
    }

    void TypeCheckVisitor::visitPointerType(PointerType *type) {
        resolveType(type->pointsTo());
    }

    void TypeCheckVisitor::visitCustomType(CustomType *type) {
        auto typeDecl = findTypeDeclaration(type);
        if (!typeDecl) {
            typeDecl = _session->findTypeDeclaration(type->name());
        }
        if (!typeDecl) {
            _session->reportError("Couldn't resolve typename.", type->sourceRange());
        }
        type->resolvesTo(typeDecl);
    }

    TypeDeclaration* TypeCheckVisitor::findTypeDeclaration(CustomType *type) {
        auto it = _typeDeclarations.find(type->name());
        if (it != _typeDeclarations.end()) {
            return (*it).second;
        }
        return nullptr;
    }
}
