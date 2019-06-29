#include "c_emit_visitor.h"
#include "ast/module.h"
#include "ast/stmt.h"
#include "ast/expr.h"

namespace klong {
    std::string CEmitVisitor::getOutput() const {
        return _outputStream.str();
    }

    void CEmitVisitor::setSession(CompilationSession* session) {
        _session = session;
    }
    void CEmitVisitor::visitModule(Module * module)
    {
        _outputStream << "#ifndef HEADERGUARD_" << module->filenameWithoutExtension() << "_H" << std::endl;
        _outputStream << "#define HEADERGUARD_" << module->filenameWithoutExtension() << "_H" << std::endl;
        
        _outputStream << "#define NULL 0" << std::endl;
        _outputStream << "#include <stdint.h>" << std::endl;
        _outputStream << "#include <stdbool.h>" << std::endl;
/*
        _outputStream << "#include <math.h>" << std::endl;
        _outputStream << "#include <stdio.h>" << std::endl;
        _outputStream << "#include <stdlib.h>" << std::endl;
        _outputStream << "#include <string.h>" << std::endl;
*/

        // include other module dependencies
        for (auto& dependants : module->dependencies()) {
            _outputStream << "#include \"" << dependants->filenameWithoutExtension() << ".c\"" << std::endl;
        }

        for (auto& stmt : module->statements()) {
            if (stmt->kind() == StatementKind::FUNCTION) {
                Function* function = static_cast<Function*>(stmt);
                // forward declaration
                if (!function->isPublic()) {
                    _outputStream << "static ";
                }
                function->functionType()->returnType()->accept(this);
                _outputStream << function->name();
                _outputStream << "(";
                auto params = function->params();
                for (uint32_t i = 0; i < params.size(); ++i) {
                    auto& param = params[i];
                    param->type()->accept(this);
                    _outputStream << param->name() << ((params.size() - 1 == i) ? "" : ", ");
                }
                if (function->functionType()->isVariadic()) {
                    _outputStream << ", ...";
                }
                _outputStream << ")";
                _outputStream << ";" << std::endl;
            }
        }

        for (auto& stmt : module->statements()) {
            stmt->accept(this);
        }

        _outputStream << "#endif " << "/* " << "HEADERGUARD_" << module->filenameWithoutExtension() << "_H" << " */" << std::endl;
    }

    void CEmitVisitor::visitBlockStmt(Block * stmt)
    {
        for (auto& childStmt : stmt->statements()) {
            _outputStream << "    ";
            childStmt->accept(this);
        }
    }
    
    void CEmitVisitor::visitExpressionStmt(Expression * stmt)
    {
        stmt->expression()->accept(this);
        _outputStream << ";" << std::endl;
    }
    
    void CEmitVisitor::visitExtDeclStmt(ExternalDeclaration * stmt)
    {
        _outputStream << "extern ";
        if (stmt->type()->kind() == TypeKind::FUNCTION) {
            auto functionType = static_cast<FunctionType*>(stmt->type());
            functionType->returnType()->accept(this);
            _outputStream << stmt->name();
            _outputStream << "(";
            auto paramTypes = functionType->paramTypes();
            for (uint32_t i = 0; i < paramTypes.size(); ++i) {
                auto& paramType = paramTypes[i];
                paramType->accept(this);
                _outputStream << ((paramTypes.size() - 1 == i) ? "" : ", ");
            }
            if (functionType->isVariadic()) {
                _outputStream << ", ...";
            }
            _outputStream << ")";

            _outputStream << ";" << std::endl;
            return;
        }
        stmt->type()->accept(this);
        _outputStream << stmt->name() << ";" << std::endl;
    }
    
    void CEmitVisitor::visitImportStmt(Import * stmt)
    {
        // Nothing to do here
    }

    void CEmitVisitor::visitParameterStmt(Parameter * stmt)
    {
        // Nothing to do here
    }

    void CEmitVisitor::visitCustomMemberStmt(CustomMember * stmt)
    {
        // Nothing to do here
    }

    void CEmitVisitor::visitFunctionStmt(Function * stmt)
    {
        if (!stmt->isPublic()) {
            _outputStream << "static ";
        }
        stmt->functionType()->returnType()->accept(this);
        _outputStream << stmt->name();
        _outputStream << "(";
        auto params = stmt->params();
        for (uint32_t i = 0; i < params.size(); ++i) {
            auto& param = params[i];
            param->type()->accept(this);
            _outputStream << param->name() << ((params.size() - 1 == i) ? "" : ", ");
        }
        if (stmt->functionType()->isVariadic()) {
            _outputStream << ", ...";
        }
        _outputStream << ")";
        _outputStream << "{" << std::endl;
        for (auto& statement : stmt->body()) {
            _outputStream << "    ";
            statement->accept(this);
        }
        _outputStream << "}" << std::endl;
    }
    
    void CEmitVisitor::visitIfStmt(If * stmt)
    {
        _outputStream << "if (";
        stmt->condition()->accept(this);
        _outputStream << ") {" << std::endl;
        stmt->thenBranch()->accept(this);
        _outputStream << "}" << std::endl;
        if (stmt->elseBranch()) {
            _outputStream << "else {" << std::endl;
            stmt->elseBranch()->accept(this);
            _outputStream << "}" << std::endl;
        }
    }
    
    void CEmitVisitor::visitReturnStmt(Return * stmt)
    {
        _outputStream << "return ";
        stmt->value()->accept(this);
        _outputStream << ";" << std::endl;
    }
    
    void CEmitVisitor::visitVarDeclStmt(VariableDeclaration * stmt)
    {
        if (stmt->isConst()) {
            _outputStream << "const ";
        }
        if (stmt->isGlobal() && !stmt->isPublic()) {
            _outputStream << "static ";
        }
        bool isArray = false;
        uint64_t arraySize = 0;
        Type* innerType = nullptr;
        auto type = stmt->type();
        if (Type::isPointer(type)) {
            auto pointerType = static_cast<PointerType*>(type);
            if (pointerType->isArray()) {
                isArray = true;
                arraySize = pointerType->size();
                innerType = pointerType->pointsTo();
            }
        }

        if (isArray) {
            innerType->accept(this);
        }
        else {
            type->accept(this);
        }
        _outputStream << " " << stmt->name();
        if (isArray) {
            _outputStream << "[" << arraySize << "]";
        }
        if (stmt->initializer()) {
            _outputStream << "= ";
            stmt->initializer()->accept(this);
        }
        _outputStream << ";" << std::endl;
    }
    
    void CEmitVisitor::visitStructDeclStmt(StructDeclaration * stmt)
    {
        _outputStream << "struct " << stmt->name() << ";" << std::endl;
        _outputStream << "struct " << stmt->name() << " {" << std::endl;
        for (auto& member : stmt->members()) {
            _outputStream << "    ";
            member->type()->accept(this);
            _outputStream << member->name() << ";" << std::endl;
        }
        _outputStream << "};" << std::endl;
    }
    
    void CEmitVisitor::visitUnionDeclStmt(UnionDeclaration * stmt)
    {
        _outputStream << "union " << stmt->name() << ";" << std::endl;
        _outputStream << "union " << stmt->name() << " {" << std::endl;
        for (auto& member : stmt->members()) {
            _outputStream << "    ";
            member->type()->accept(this);
            _outputStream << member->name() << ";" << std::endl;
        }
        _outputStream << "};" << std::endl;
    }
    
    void CEmitVisitor::visitEnumDeclStmt(EnumDeclaration * stmt)
    {
        _outputStream << "enum " << stmt->name() << ";" << std::endl;
        _outputStream << "enum " << stmt->name() << " {" << std::endl;
        const auto& values = stmt->values();
        for (int i = 0; i < values.size(); ++i) {
            _outputStream << "    " << values[i] << ((values.size() - 1 == i) ? "" : ",") << std::endl;
        }
        _outputStream << "};" << std::endl;
    }
    
    void CEmitVisitor::visitWhileStmt(While * stmt)
    {
        _outputStream << "while ( ";
        stmt->condition()->accept(this);
        _outputStream << " ) {" << std::endl;
        stmt->body()->accept(this);
        _outputStream << "}" << std::endl;
    }
    
    void CEmitVisitor::visitForStmt(For * stmt)
    {
        _outputStream << "for ( ";
        stmt->initializer()->accept(this);
        stmt->condition()->accept(this);
        _outputStream << ";";
        stmt->increment()->accept(this);
        _outputStream << " ) {" << std::endl;
        stmt->body()->accept(this);
        _outputStream << "}" << std::endl;
    }
    
    void CEmitVisitor::visitBreakStmt(Break * stmt)
    {
        _outputStream << "break;" << std::endl;
    }
    
    void CEmitVisitor::visitContinueStmt(Continue * stmt)
    {
        _outputStream << "continue;" << std::endl;
    }
    
    void CEmitVisitor::visitDeferStmt(Defer * stmt)
    {
    }
    
    void CEmitVisitor::visitCommentStmt(Comment * expr)
    {
        std::istringstream commentStream(expr->text());
        for (std::string line; std::getline(commentStream, line);) {
            _outputStream << "// " << line << std::endl;
        }
    }
    
    void CEmitVisitor::visitAssignExpr(Assign * expr)
    {
        expr->target()->accept(this);
        _outputStream << " = ";
        expr->value()->accept(this);
    }
    
    void CEmitVisitor::visitBinaryExpr(Binary * expr)
    {
        _outputStream << "( ";
        expr->left()->accept(this);
        switch (expr->op()) {
        case BinaryOperation::PLUS:
            _outputStream << " + ";
            break;
        case BinaryOperation::MINUS:
            _outputStream << " - ";
            break;
        case BinaryOperation::MULTIPLICATION:
            _outputStream << " * ";
            break;
        case BinaryOperation::DIVISION:
            _outputStream << " / ";
            break;
        case BinaryOperation::MODULO:
            _outputStream << " % ";
            break;
        case BinaryOperation::LSL:
            _outputStream << " << ";
            break;
        case BinaryOperation::LSR:
            _outputStream << " >> ";
            break;
        case BinaryOperation::ASR:
            // POSSIBLE BUG: we need to create a temporary signed variable then shift
            _outputStream << " >> ";
            break;
        case BinaryOperation::GREATER_THAN:
            _outputStream << " > ";
            break;
        case BinaryOperation::GREATER_EQUAL:
            _outputStream << " >= ";
            break;
        case BinaryOperation::LESS_THAN:
            _outputStream << " < ";
            break;
        case BinaryOperation::LESS_EQUAL:
            _outputStream << " <= ";
            break;
        case BinaryOperation::EQUALITY:
            _outputStream << " == ";
            break;
        case BinaryOperation::INEQUALITY:
            _outputStream << " != ";
            break;
        case BinaryOperation::XOR:
            _outputStream << " ^ ";
            break;
        case BinaryOperation::AND:
            _outputStream << " & ";
            break;
        case BinaryOperation::OR:
            _outputStream << " | ";
            break;
        }
        expr->right()->accept(this);
        _outputStream << ") ";
    }
    
    void CEmitVisitor::visitCallExpr(Call * expr)
    {
        _outputStream << "( ";
        expr->callee()->accept(this);
        _outputStream << "( ";
        auto args = expr->args();
        for (uint32_t i = 0; i < args.size(); ++i) {
            auto& arg = args[i];
            arg->accept(this);
            if (args.size() - 1 != i) {
                _outputStream << ", ";
            }
        }
        _outputStream << ")) ";
    }
    
    void CEmitVisitor::visitGroupingExpr(Grouping * expr)
    {
        _outputStream << "( ";
        expr->expression()->accept(this);
        _outputStream << ") ";
    }
    
    void CEmitVisitor::visitSubscriptExpr(Subscript * expr)
    {
        _outputStream << "(";
        expr->target()->accept(this);
        _outputStream << "[";
        expr->index()->accept(this);
        _outputStream << "]) ";
    }
    
    void CEmitVisitor::visitMemberAccessExpr(MemberAccess * expr)
    {
        expr->target()->accept(this);
        if (Type::isPointer(expr->target()->type())) {
            _outputStream << "->";
        }
        else {
            _outputStream << ".";
        }
        _outputStream << expr->value() << " ";
    }
    
    void CEmitVisitor::visitEnumAccessExpr(EnumAccess * expr)
    {
        _outputStream << expr->value() << " ";
    }
    
    void CEmitVisitor::visitLogicalExpr(Logical * expr)
    {
        _outputStream << "(";
        switch (expr->op()) {
        case LogicalOperation::AND:
            _outputStream << "&&";
            break;
        case LogicalOperation::OR:
            _outputStream << "||";
            break;
        }
        expr->right()->accept(this);
        _outputStream << ") ";
    }
    
    void CEmitVisitor::visitUnaryExpr(Unary * expr)
    {
        _outputStream << "(";
        switch (expr->op()) {
        case UnaryOperation::NOT:
            _outputStream << "!";
            break;
        case UnaryOperation::MINUS:
            _outputStream << "-";
            break;
        case UnaryOperation::DEREF:
            _outputStream << "*";
            break;
        case UnaryOperation::ADDRESS_OF:
            _outputStream << "&";
            break;
        }
        expr->right()->accept(this);
        _outputStream << ") ";
    }
    
    void CEmitVisitor::visitSizeOfExpr(SizeOf * expr)
    {
        _outputStream << "sizeof( ";
        expr->right()->accept(this);
        _outputStream << ")";
    }
    
    void CEmitVisitor::visitCastExpr(Cast * expr)
    {
        _outputStream << "( ( ";
        expr->targetType()->accept(this);
        _outputStream << ") ";
        expr->right()->accept(this);
        _outputStream << ") ";
    }
    
    void CEmitVisitor::visitVariableExpr(Variable * expr)
    {
        _outputStream << expr->name() << " ";
    }
    
    void CEmitVisitor::visitNumberLiteral(NumberLiteral * expr)
    {
        switch (static_cast<PrimitiveType*>(expr->type())->type()) {
        case PrimitiveTypeKind::I32:
            _outputStream<< expr->i32();
            break;
        case PrimitiveTypeKind::I64:
            _outputStream << expr->i64();
            break;
        case PrimitiveTypeKind::U32:
            _outputStream << expr->u32();
            break;
        case PrimitiveTypeKind::U64:
            _outputStream << expr->u64();
            break;
        case PrimitiveTypeKind::F32:
            _outputStream << expr->f32();
            break;
        case PrimitiveTypeKind::F64:
            _outputStream << expr->f64();
            break;
        default:
            assert(false);
            break;
        }
        _outputStream << " ";
    }
    
    void CEmitVisitor::visitBoolLiteral(BoolLiteral * expr)
    {
        _outputStream << expr->value() ? "true " : "false ";
    }
    
    void CEmitVisitor::visitNullLiteral(NullLiteral * expr)
    {
        _outputStream << "NULL ";
    }
    
    void replaceAll(std::string& str, const std::string& from, const std::string& to) {
        if (from.empty())
            return;
        size_t start_pos = 0;
        while ((start_pos = str.find(from, start_pos)) != std::string::npos) {
            str.replace(start_pos, from.length(), to);
            start_pos += to.length(); // In case 'to' contains 'from', like replacing 'x' with 'yx'
        }
    }

    void CEmitVisitor::visitStringLiteral(StringLiteral * expr)
    {
        auto stringValue = expr->value();
        replaceAll(stringValue, "\'", "\\'");
        replaceAll(stringValue, "\"", "\\\"");
        replaceAll(stringValue, "\?", "\\\?");
        replaceAll(stringValue, "\\", "\\\\");
        replaceAll(stringValue, "\a", "\\a");
        replaceAll(stringValue, "\b", "\\b");
        replaceAll(stringValue, "\f", "\\f");
        replaceAll(stringValue, "\n", "\\n");
        replaceAll(stringValue, "\r", "\\r");
        replaceAll(stringValue, "\t", "\\t");
        replaceAll(stringValue, "\v", "\\v");
        replaceAll(stringValue, "\0", "\\0");
        _outputStream << "\"" << stringValue << "\" ";
    }
    
    void CEmitVisitor::visitCharacterLiteral(CharacterLiteral * expr)
    {
        auto stringValue = std::string("") + expr->value();
        replaceAll(stringValue, "\'", "\\'");
        replaceAll(stringValue, "\"", "\\\"");
        replaceAll(stringValue, "\?", "\\\?");
        replaceAll(stringValue, "\\", "\\\\");
        replaceAll(stringValue, "\a", "\\a");
        replaceAll(stringValue, "\b", "\\b");
        replaceAll(stringValue, "\f", "\\f");
        replaceAll(stringValue, "\n", "\\n");
        replaceAll(stringValue, "\r", "\\r");
        replaceAll(stringValue, "\t", "\\t");
        replaceAll(stringValue, "\v", "\\v");
        replaceAll(stringValue, "\0", "\\0");
        _outputStream << "'" << expr->value() << "' ";
    }
    
    void CEmitVisitor::visitArrayLiteral(ArrayLiteral * expr)
    {
    }

    void CEmitVisitor::visitFunctionType(FunctionType * type)
    {
        type->returnType()->accept(this);
        _outputStream << "(*" << _functionTypename << ")(";
        auto paramTypes = type->paramTypes();
        for (uint32_t i = 0; i < paramTypes.size(); ++i) {
            auto& paramType = paramTypes[i];
            paramType->accept(this);
            _outputStream << ((paramTypes.size() - 1 == i) ? "" : ", ");
        }
        if (type->isVariadic()) {
            _outputStream << ", ...";
        }
        _outputStream << ")";
    }

    void CEmitVisitor::visitPrimitiveType(PrimitiveType * type)
    {
        switch (type->type()) {
        case PrimitiveTypeKind::VOID:
            _outputStream << "void ";
            break;
        case PrimitiveTypeKind::BOOL:
            _outputStream << "bool ";
            break;
        case PrimitiveTypeKind::I8:
            _outputStream << "int8_t ";
            break;
        case PrimitiveTypeKind::I16:
            _outputStream << "int16_t ";
            break;
        case PrimitiveTypeKind::I32:
            _outputStream << "int32_t ";
            break;
        case PrimitiveTypeKind::I64:
            _outputStream << "int64_t ";
            break;
        case PrimitiveTypeKind::U8:
            _outputStream << "uint8_t ";
            break;
        case PrimitiveTypeKind::U16:
            _outputStream << "uint16_t ";
            break;
        case PrimitiveTypeKind::U32:
            _outputStream << "uint32_t ";
            break;
        case PrimitiveTypeKind::U64:
            _outputStream << "uint64_t ";
            break;
        case PrimitiveTypeKind::F32:
            _outputStream << "float ";
            break;
        case PrimitiveTypeKind::F64:
            _outputStream << "double ";
            break;
        }
    }

    void CEmitVisitor::visitPointerType(PointerType * type)
    {
        auto pointsTo = type->pointsTo();
        if (pointsTo->kind() == TypeKind::FUNCTION) {
            pointsTo->accept(this);
            return;
        }
        pointsTo->accept(this);
        _outputStream << "* ";
    }

    void CEmitVisitor::visitCustomType(CustomType * type)
    {
        auto resolvesTo = type->resolvesTo();
        if (resolvesTo->typeDeclarationKind() == TypeDeclarationKind::STRUCT) {
            _outputStream << "struct ";
        } else if (resolvesTo->typeDeclarationKind() == TypeDeclarationKind::UNION) {
            _outputStream << "union ";
        } else if (resolvesTo->typeDeclarationKind() == TypeDeclarationKind::ENUM) {
            _outputStream << "enum ";
        }
        _outputStream << type->name() << " ";
    }
}