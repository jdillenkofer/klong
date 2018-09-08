#include "environment.h"
#include "terp_exception.h"

namespace klong {

    std::any Environment::get(Stmt* resolveStmt) {
        auto value = _values.find(resolveStmt);
        if (value != _values.end()) {
            return value->second;
        }

        if (_enclosing != nullptr) {
            return _enclosing->get(resolveStmt);
        }

        throw TerpException("Couldn't resolve an value in the environment. This is probably an error in the interpreter.");
    }

    void Environment::define(Stmt* resolveStmt, std::any value) {
        _values.insert(std::pair<Stmt*, std::any>(resolveStmt, value));
    }

    void Environment::assign(Stmt* resolveStmt, std::any value) {
        auto it = _values.find(resolveStmt);
        if (it != _values.end()) {
            it->second = value;
            return;
        }

        if (_enclosing != nullptr) {
            _enclosing->assign(resolveStmt, value);
            return;
        }

        throw TerpException("Undefined variable assigned in interpreter. This is probably an error in the interpreter.");
    }
}