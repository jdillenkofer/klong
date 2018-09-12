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
            Module(std::string filename, std::vector<StmtPtr>&& statements):
                _statements(statements), _filename(filename) {

            }

            virtual void accept(Visitor* visitor) {
                visitor->visitModule(this);
            };

            std::vector<StmtPtr> statements() const {
                return _statements;
            }

            std::string filename() const {
                return _filename;
            }

        private:
            std::vector<StmtPtr> _statements;
            std::string _filename;
    };

    using ModulePtr = std::shared_ptr<Module>;
}