#pragma once

#include <string>
#include <fstream>
#include <sstream>

namespace klong {
    class SourceFile {
        public:
        SourceFile(std::string path):
            _path(std::move(path)) {
        
        }

        SourceFile(std::string path, std::string code):
            _path(std::move(path)), _code(std::move(code)) {

        }

        bool loadFromFile() {
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

        const std::string& code() const {
            return _code;
        }

        const std::string& path() const {
            return _path;
        }

        private:
        std::string _path;
        std::string _code;
    };
}