#include "ast/string_helper.h"

namespace klong {
    std::string to_string(bool b) {
        return b ? "true" : "false";
    }

    std::string to_string(BinaryOperation bOp) {
        switch(bOp) {
            case BinaryOperation::PLUS:
                return "PLUS";
            case BinaryOperation::MINUS:
                return "MINUS";
            case BinaryOperation::MULTIPLICATION:
                return "MULTIPLICATION";
            case BinaryOperation::DIVISION:
                return "DIVISION";
            case BinaryOperation::MODULO:
                return "MODULO";
            case BinaryOperation::LSL:
                return "LSL";
            case BinaryOperation::LSR:
                return "LSR";
            case BinaryOperation::ASR:
                return "ASR";
            case BinaryOperation::GREATER_THAN:
                return "GREATER THAN";
            case BinaryOperation::GREATER_EQUAL:
                return "GREATER EQUAL";
            case BinaryOperation::LESS_THAN:
                return "LESS THAN";
            case BinaryOperation::LESS_EQUAL:
                return "LESS EQUAL";
            case BinaryOperation::EQUALITY:
                return "EQUALITY";
            case BinaryOperation::INEQUALITY:
                return "INEQUALITY";
            case BinaryOperation::AND:
                return "AND";
            case BinaryOperation::XOR:
                return "XOR";
            case BinaryOperation::OR:
                return "OR";
            default:
                return "UNDEFINED";
        }
    }

    std::string to_string(LogicalOperation lOp) {
        switch (lOp) {
            case LogicalOperation::OR:
                return "OR";
            case LogicalOperation::AND:
                return "AND";
            default:
                return "UNDEFINED";
        }
    }

    std::string to_string(UnaryOperation uOp) {
        switch (uOp) {
            case UnaryOperation::NOT:
                return "NOT";
            case UnaryOperation::MINUS:
                return "MINUS";
            default:
                return "UNDEFINED";
        }
    }
}