#pragma once

#include <any>
#include <memory>
#include <map>
#include "parser/stmt.h"

namespace klong {
    class Environment {
        public:
            Environment(): _enclosing(nullptr) {

            }

            Environment(std::shared_ptr<Environment> enclosing): _enclosing(std::move(enclosing)) {

            }

            std::any get(Stmt* resolveStmt);
            void assign(Stmt* resolveStmt, std::any value);
            void define(Stmt* resolveStmt, std::any value);

        private:
            std::shared_ptr<Environment> _enclosing;
            std::map<Stmt*, std::any> _values;
    };
}