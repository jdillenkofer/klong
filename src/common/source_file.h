#pragma once

#include <string>
#include <fstream>
#include <sstream>

namespace klong {
    class SourceFile {
        public:
        SourceFile(const std::string& path) : _path(path) {
        }

        inline bool load() {
            std::ifstream file(_path);
            std::stringstream input_buffer;
            if (!file) {
                return false;
            }

            input_buffer << file.rdbuf();
            file.close();
            _code = input_buffer.str();
            return true;
        }

        inline const std::string& code() const {
            return _code;
        }

        inline const std::string& path() const {
            return _path;
        }

        private:
        std::string _path;
        std::string _code;
    };
}