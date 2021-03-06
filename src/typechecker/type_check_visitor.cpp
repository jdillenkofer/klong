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
            _session->addError(
                    CompilationError(typeDeclarationStmt->sourceRange(),
                            "Type '" + typeDeclarationStmt->name() + "' already declared."));
        }
        if (typeDeclarationStmt->isPublic()) {
            auto isAlreadyDeclared = !_session->declareType(typeDeclarationStmt->name(), typeDeclarationStmt);
            if (isAlreadyDeclared) {
                _session->addError(
                        CompilationError(typeDeclarationStmt->sourceRange(),
                                         "Type '" + typeDeclarationStmt->name() + "' already declared."));
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
        check(stmt->body());
        auto primType = static_cast<PrimitiveType*>(stmt->functionType()->returnType());
        if (!_returnsValue && primType != nullptr && !primType->isVoid()) {
            _session->addError(
                    CompilationError(stmt->sourceRange(),
                            "Control-flow reaches end of non-void function "
                            + stmt->name() + "."));
        }
        currentFunction = previousFunction;
    }

    void TypeCheckVisitor::visitParameterStmt(Parameter* stmt) {
        if (stmt->type()->kind() == TypeKind::FUNCTION) {
            _session->addError(
                    CompilationError(stmt->sourceRange(), "Parameters of type functionType are not allowed."));
        }
        if (stmt->type()->kind() == TypeKind::POINTER) {
            auto pointerType = static_cast<PointerType*>(stmt->type());
            if (pointerType->isArray()) {
                _session->addError(
                    CompilationError(stmt->sourceRange(), "Parameters of type array are not allowed."));
            }
        }
    }

    void TypeCheckVisitor::visitIfStmt(If* stmt) {
        check(stmt->condition());
        if (!Type::isBoolean(stmt->condition()->type())) {
            _session->addError(
                    CompilationError(stmt->condition()->sourceRange(), "Expect bool condition in if-statement."));
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
                    _session->addError(
                        CompilationError(stmt->sourceRange(), "Expect return statement type to match the function returnType."));
                }
            }
            _returnsValue = true;
        }
    }

    void TypeCheckVisitor::visitVarDeclStmt(VariableDeclaration* stmt) {
        if (currentFunction != nullptr && stmt->isPublic()) {
            _session->addError(
                    CompilationError(stmt->sourceRange(), "Pub keyword not allowed in front of local variable."));
        }
        check(stmt->initializer());

        if (stmt->type() == nullptr) {
            auto clonedInitType = std::shared_ptr<Type>(stmt->initializer()->type()->clone());

            auto initAsPointerType = dynamic_cast<PointerType*>(stmt->initializer()->type());

            // propagate array type meta info
            if (initAsPointerType && initAsPointerType->isArray()) {
                auto clonedInitasPointerType = dynamic_cast<PointerType*>(clonedInitType.get());
                clonedInitasPointerType->isArray(true);
                clonedInitasPointerType->size(initAsPointerType->size());
            }

            stmt->type(clonedInitType);
        } else {
            resolveType(stmt->type());
            auto stmtAsPointerType = dynamic_cast<PointerType*>(stmt->type());
            if (stmtAsPointerType && stmtAsPointerType->isArray()
                && stmt->initializer() && stmt->initializer()->kind() != ExprKind::LITERAL) {
                if (!Type::isVoidPtrCast(stmt->initializer(), stmt->type())) {
                    _session->addError(
                        CompilationError(stmt->sourceRange(), "initializer of arrays can only contain array literals."));
                }
            }
            if (stmt->initializer() && !stmt->type()->isEqual(stmt->initializer()->type())) {
                if (!Type::isVoidPtrCast(stmt->initializer(), stmt->type())) {
                    _session->addError(
                        CompilationError(stmt->sourceRange(), "initializerType doesn't match declaration type."));
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
                    _session->addError(
                        CompilationError(value->sourceRange(), "Self referential member definitions are not allowed."));
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
            _session->addError(
                    CompilationError(stmt->condition()->sourceRange(), "while condition expects bool type."));
        }
        check(stmt->body());
    }

    void TypeCheckVisitor::visitForStmt(For* stmt) {
        check(stmt->initializer());
        check(stmt->condition());
        if (!Type::isBoolean(stmt->condition()->type())) {
            _session->addError(
                    CompilationError(stmt->condition()->sourceRange(), "for condition expects bool type."));
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
                _session->addError(
                    CompilationError(expr->value()->sourceRange(), "Expect valid type in assignment."));
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
				_session->addError(
				        CompilationError(expr->sourceRange(), "Illegal type in arithmetic operation."));
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

                _session->addError(
                        CompilationError(expr->sourceRange(), "Comparisons must be of the same type."));
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
                _session->addError(
                        CompilationError(expr->sourceRange(), "Illegal type in binary operation"));
                break;
            }
            default:
                _session->addError(
                        CompilationError(expr->sourceRange(), "Illegal binary op."));
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
                    _session->addError(
                        CompilationError(expr->sourceRange(), "Call Expr doesn't match function signature."));
                }
                expr->type(std::shared_ptr<Type>(functionType->returnType()->clone()));
                return;
            }
        }
        _session->addError(
                CompilationError(expr->sourceRange(), "Callee doesn't resolve to function pointer expression."));
    }

    void TypeCheckVisitor::visitGroupingExpr(Grouping* expr) {
        check(expr->expression());
        expr->type(std::shared_ptr<Type>(expr->expression()->type()->clone()));
    }

	void TypeCheckVisitor::visitSubscriptExpr(Subscript* expr) {
		check(expr->target());
        if (expr->target()->type()->kind() != TypeKind::POINTER) {
            _session->addError(
                CompilationError(expr->sourceRange(),
                    "Illegal target type for subscript expr. Target has to be of type pointer."));
            return;
        }
		check(expr->index());
        if (expr->index()->type()->kind() != TypeKind::PRIMITIVE) {
            _session->addError(
                CompilationError(expr->sourceRange(),
                    "Index of subscript operator has to be numeric."));
            return;
        }
		auto pointerType = static_cast<PointerType*>(expr->target()->type());
		auto numberType = static_cast<PrimitiveType*>(expr->index()->type());
		if (!numberType->isInteger()) {
			_session->addError(
			        CompilationError(expr->sourceRange(),
			                "Index of subscript operator has to be numeric."));
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
            _session->addError(
                    CompilationError(expr->sourceRange(),
                            "MemberAccess target is not a custom type."));
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
                    _session->addError(
                            CompilationError(
                                    expr->sourceRange(),
                                    "MemberAccess target does not have such a member element."));
                } else {
                    expr->type(std::shared_ptr<Type>(memberPtr->type()->clone()));
                }
                break;
            }
			case TypeDeclarationKind::ENUM: {
				_session->addError(
				        CompilationError(expr->sourceRange(),
				                "MemberAccess target does not allow enum types. Try to use the '::' operator instead."));
				break;
			}
            default:
                assert(false);
        }
    }

	void TypeCheckVisitor::visitEnumAccessExpr(EnumAccess* expr) {
		resolveType(expr->target());
		auto resolvedTypeDecl = expr->target()->resolvesTo();
		if (resolvedTypeDecl && resolvedTypeDecl->typeDeclarationKind() != TypeDeclarationKind::ENUM) {
			_session->addError(
			        CompilationError(expr->target()->sourceRange(), "Expect enum type."));
		} else {
			auto enumValues = static_cast<EnumDeclaration*>(resolvedTypeDecl)->values();
			auto it = std::find(enumValues.begin(), enumValues.end(), expr->value());
			if (it == enumValues.end()) {
				_session->addError(
				        CompilationError(expr->sourceRange(), "No such value " + expr->value() + " in enum type " + expr->target()->name() + "."));
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
            _session->addError(
                    CompilationError(expr->left()->sourceRange(), "Expect boolean expr."));
        }
        check(expr->right());
        auto rightType = expr->right()->type();
        if (!rightType) {
            return;
        }
        if (!Type::isBoolean(rightType)) {
            _session->addError(
                    CompilationError(expr->right()->sourceRange(), "Expect boolean expr."));
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
            _session->addError(
                    CompilationError(expr->sourceRange(), "'!' expects boolean expression."));
        }

        if (expr->op() == UnaryOperation::MINUS && !Type::isInteger(rightType)) {
            _session->addError(
                    CompilationError(expr->sourceRange(), "Unary '-' expects number expression."));
        }

        if (expr->op() == UnaryOperation::DEREF) {
            if (!Type::isPointer(rightType)) {
                _session->addError(
                        CompilationError(expr->sourceRange(), "Deref expects pointer type."));
                return;
            }
            auto pointerType = static_cast<PointerType*>(rightType);
            if (pointerType->pointsTo()->kind() == TypeKind::FUNCTION) {
                _session->addError(
                        CompilationError(expr->sourceRange(),
                                "Deref expects non function pointer type."));
            }
            expr->type(std::shared_ptr<Type>(pointerType->pointsTo()->clone()));
            return;
        }

        if (expr->op() == UnaryOperation::ADDRESS_OF) {
            auto variable = dynamic_cast<Variable*>(expr->right());
            if (variable == nullptr) {
                _session->addError(
                        CompilationError(expr->sourceRange(),
                                "Can only get address of variable expressions."));
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
                    _session->addError(
                            CompilationError(expr->sourceRange(),
                                               "Can not get address of function. Function names are already pointers."));
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
            _session->addError(
                    CompilationError(expr->right()->sourceRange(),
                            "Can not get sizeof a function type. Did you mean sizeof<ptr type>?"));
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
            _session->addError(
                    CompilationError(sourceType->sourceRange(),
                            "Can not cast from void type."));
        }
        if (sourceType->kind() == TypeKind::CUSTOM) {
            _session->addError(
                    CompilationError(expr->targetType()->sourceRange(),
                                     "Can not cast from custom type."));
        }
        auto targetType = std::shared_ptr<Type>(expr->targetType()->clone());
        if (expr->targetType()->kind() == TypeKind::FUNCTION) {
            _session->addError(
                    CompilationError(expr->targetType()->sourceRange(),
                            "Can not cast to function type. Did you mean ptr to function?"));
        }
        if (expr->targetType()->kind() == TypeKind::CUSTOM) {
            _session->addError(
                    CompilationError(expr->targetType()->sourceRange(),
                            "Can not cast to custom type. Did you mean ptr to custom type?"));
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
                _session->addError(
                        CompilationError(expr->sourceRange(), "Variable resolves to invalid kind."));
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
                throw CompilationError(expr->sourceRange(), "Not all array values are of the same type.");
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
                _session->addError(
                        CompilationError(param->sourceRange(), "Parameters of type functionType are not allowed."));
            }
            if (param->kind() == TypeKind::PRIMITIVE) {
                if (Type::isVoid(param)) {
                    _session->addError(
                            CompilationError(param->sourceRange(), "Parameters of type void are not allowed."));
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
            _session->addError(
                    CompilationError(type->sourceRange(), "Couldn't resolve typename."));
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
