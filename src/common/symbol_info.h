#pragma once

#include "ast/stmt.h"

namespace klong {
    enum class DeclarationType {
        CONST,
        LET,
        PARAM,
        FUNCTION
    };

    struct SymbolInfo {
        std::string owningModulepath;
        Stmt* declarationStmt;
        DeclarationType declarationType;
        bool initialized = false;
    };
}