#pragma once

#include <string>
#include <fstream>
#include <sstream>
#include <filesystem>

namespace klong {
    class SourceFile {
    public:
        explicit SourceFile(std::string path):
            _absolutepath(std::filesystem::absolute(std::move(path)).string()),
			_filename(std::filesystem::path(_absolutepath).filename().string()) {
        }

        SourceFile(std::string path, std::string code):
            _absolutepath(std::filesystem::absolute(std::move(path)).string()),
            _filename(std::filesystem::path(_absolutepath).filename().string()),
            _code(std::move(code)) {
        }

        bool loadFromFile() {
            std::ifstream file(_absolutepath);
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

        const std::string& absolutepath() const {
            return _absolutepath;
        }

        const std::string& filename() const {
            return _filename;
        }

    private:
        std::string _absolutepath;
        std::string _filename;
        std::string _code;
    };
}