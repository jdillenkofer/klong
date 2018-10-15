#pragma once

#include <string>
#include <fstream>
#include <sstream>
#include <filesystem>

namespace klong {
    class SourceFile {
    public:
        explicit SourceFile(std::string path):
            _path(std::move(path)),
			_filename(std::filesystem::path(_path).filename().string()) {
        }

        SourceFile(std::string path, std::string code):
            _path(std::move(path)),
            _filename(std::filesystem::path(_path).filename().string()),
            _code(std::move(code)) {
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

        const std::string& filename() const {
            return _filename;
        }

    private:
        std::string _path;
        std::string _filename;
        std::string _code;
    };
}