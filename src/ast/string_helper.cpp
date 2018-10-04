#include "ast/string_helper.h"

namespace klong {
    std::string to_string(bool b) {
        return b ? "true" : "false";
    }

    std::string to_string(BinaryOperation bOp) {
        switch(bOp) {
            case BinaryOperation::PLUS:
                return "+";
            case BinaryOperation::MINUS:
                return "-";
            case BinaryOperation::MULTIPLICATION:
                return "*";
            case BinaryOperation::DIVISION:
                return "/";
            case BinaryOperation::MODULO:
                return "%";
            case BinaryOperation::LSL:
                return "lsl";
            case BinaryOperation::LSR:
                return "lsr";
            case BinaryOperation::ASR:
                return "asr";
            case BinaryOperation::GREATER_THAN:
                return "&#62;";
            case BinaryOperation::GREATER_EQUAL:
                return "&#62;=";
            case BinaryOperation::LESS_THAN:
                return "&#60;";
            case BinaryOperation::LESS_EQUAL:
                return "&#60;=";
            case BinaryOperation::EQUALITY:
                return "==";
            case BinaryOperation::INEQUALITY:
                return "!=";
            case BinaryOperation::AND:
                return "&";
            case BinaryOperation::XOR:
                return "^";
            case BinaryOperation::OR:
                return "&#124;";
            default:
                return "UNDEFINED";
        }
    }

    std::string to_string(LogicalOperation lOp) {
        switch (lOp) {
            case LogicalOperation::AND:
                return "&&";
            case LogicalOperation::OR:
                return "||";
            default:
                return "UNDEFINED";
        }
    }

    std::string to_string(UnaryOperation uOp) {
        switch (uOp) {
            case UnaryOperation::NOT:
                return "!";
            case UnaryOperation::MINUS:
                return "-";
            default:
                return "UNDEFINED";
        }
    }
}