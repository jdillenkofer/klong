#pragma once

#include "visitor.h"
#include "lexer/token.h"

#include <vector>
#include <memory>

namespace klong {
    class Stmt;
    using StmtPtr = std::shared_ptr<Stmt>;
    
    class Module {
    public:
        Module(std::string filename, std::vector<StmtPtr>&& statements):
            _statements(statements), _filename(std::move(filename)) {
        }

        virtual void accept(Visitor* visitor) {
            visitor->visitModule(this);
        }

        std::vector<StmtPtr> statements() const {
            return _statements;
        }

        std::string filename() const {
            return _filename;
        }

        std::string filenameNoExt() const {
            std::stringstream filenameNoExt;
            size_t n = 0;
            while (n < _filename.size()) {
                char c = _filename[n];
                if (n > 0 && c == '.') {
                    break;
                }
                filenameNoExt << c;
                n++;
            }
            return filenameNoExt.str();
        }

    private:
        std::vector<StmtPtr> _statements;
        std::string _filename;
    };

    using ModulePtr = std::shared_ptr<Module>;
}