#pragma once

#include <memory>

namespace klong {
    template <typename successType, typename errorType> class Result {
    public:
        static Result<successType, errorType> fromError(errorType&& error) {
            return Result(std::move(error));
        }

        static Result<successType, errorType> from(successType&& val) {
            return Result(std::move(val));
        }

        Result() = default;

        successType success() const {
            return _val;
        }

        void setSuccess(successType&& val) {
            _val = val;
        }

        bool isError() const {
            return _isError;
        }

        errorType error() const {
            return _error;
        }

        void setError(errorType&& err) {
            _isError = true;
            _error = err;
        }
    private:
        explicit Result(successType&& val): _val(val) {
        }

        explicit Result(errorType&& error): _error(error), _isError(true) {
        }
    private:
        bool _isError = false;
        errorType _error;
        successType _val;
    };
}