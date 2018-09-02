#pragma once

#include "../lexer/token.h"
#include "stmt.h"

namespace klong {
    class ParseException : public std::exception {
        public:
            ParseException(Token token, std::string message):
                _token(token), _message(message) {

            }

            const char* what () const throw () {
                return _message.c_str();
            }

        private:
            Token _token;
            std::string _message;
    };

    class IParser {
        public:
            virtual ~IParser() = default;
            virtual std::vector<StmtPtr> parse();
    };
}