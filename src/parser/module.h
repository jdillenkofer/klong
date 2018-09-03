#pragma once

#include "visitor.h"
#include "../lexer/token.h"

#include <vector>
#include <memory>


namespace klong {
    class Stmt;
    using StmtPtr = std::shared_ptr<Stmt>;
    
    class Module {
        public:
            Module(std::vector<StmtPtr>&& statements):
                _statements(statements) {

            }
            virtual void accept(Visitor* visitor) {
                visitor->visitModule(this);
            };
        private:
            std::vector<StmtPtr> _statements;
    };

    using ModulePtr = std::shared_ptr<Module>;
}