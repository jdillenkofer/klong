#pragma once

#include <exception>
#include <string>

namespace klong {
    class TerpException : public std::exception {
        public:
            TerpException(std::string message): _message(std::move(message)) {

            }

            const char* what () const throw () {
                return _message.c_str();
            }

        private:
            std::string _message;
    };
}
