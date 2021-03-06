#pragma once

#include "visitor.h"
#include "lexer/token.h"

#include <vector>
#include <memory>
#include <filesystem>

namespace klong {
    class Stmt;
    using StmtPtr = std::shared_ptr<Stmt>;
    
    class Module {
    public:
        Module(std::string path, std::string filename):
			_absolutepath(std::filesystem::absolute(std::move(path)).lexically_normal().string()),
			_parentpath(std::filesystem::path(_absolutepath).parent_path().string()),
			_filename(std::move(filename)) {
        }

        virtual void accept(StmtVisitor* visitor) {
            visitor->visitModule(this);
        }

        std::vector<Stmt*> statements() const {
            std::vector<Stmt*> stmts;
            for(auto& stmt : _statements) {
                stmts.push_back(stmt.get());
            }
            return stmts;
        }

        void addStatements(std::vector<StmtPtr>&& stmts) {
            _statements = stmts;
        }

		std::string absolutepath() const {
			return _absolutepath;
		}

		std::string parentpath() const {
            return _parentpath;
        }

        std::string filename() const {
            return _filename;
        }

        std::string filenameWithoutExtension() const {
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

        std::vector<std::shared_ptr<Module>> dependencies() const {
            return _dependencies;
        }

        bool hasDependency(std::string absoluteModulePath) {
            for (auto& dependency : _dependencies) {
                if (dependency->absolutepath() == absoluteModulePath) {
                    return true;
                }
            }
            return false;
        }

        void addDependency(std::shared_ptr<Module> module) {
            _dependencies.emplace_back(module);
        }

    private:
        std::vector<std::shared_ptr<Module>> _dependencies;
        std::vector<StmtPtr> _statements;
		std::string _absolutepath;
		std::string _parentpath;
        std::string _filename;
    };

    using ModulePtr = std::shared_ptr<Module>;
}