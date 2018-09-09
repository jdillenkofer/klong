#pragma once

#include <memory>
#include "../parser/stmt.h"
#include "environment.h"
#include "treewalk_terp.h"

namespace klong {
    class KlongFunction {
        public:
            KlongFunction(Function* declaration, std::shared_ptr<Environment> closure):
                _declaration(declaration), _closure(std::move(closure)) {

            }

            std::any call(TreewalkTerp* terp, std::vector<std::any>&& args) {
                auto environment = std::make_shared<Environment>(_closure);

                for(size_t i = 0; i < _declaration->params().size(); i++) {
                    environment->define(_declaration->params()[i].get(), args[i]);
                }

                try {
                    terp->executeBlock(_declaration->body(), environment);
                } catch(const ReturnWrapper& returnWrapper) {
                    return returnWrapper.value();
                }
                return nullptr;
            }
        private:
            std::shared_ptr<Function> _declaration;
            std::shared_ptr<Environment> _closure;
    };
}