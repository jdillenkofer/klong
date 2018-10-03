#pragma once

#include <string>
#include "expr.h"

namespace klong {
    std::string to_string(bool b);
    std::string to_string(BinaryOperation bOp);
    std::string to_string(LogicalOperation lOp);
    std::string to_string(UnaryOperation uOp);
}