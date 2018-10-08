#pragma once

#include <vector>
#include <optional>

namespace klong {
    template <typename successType, typename errorType> class Result {
    public:
        static Result<successType, errorType> fromErrors(std::vector<errorType>&& errors) {
            return Result(std::move(errors));
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

        bool hasErrors() const {
            return _errors.size() > 0;
        }

        std::vector<errorType> getErrors() const {
            return _errors;
        }

        std::optional<errorType> getFirstError() const {
            if (hasErrors()) {
                return std::make_optional(_errors[0]);
            }
            return {};
        }

        void addError(errorType&& error) {
            _errors.push_back(error);
        }

        void reset() {
            _errors.clear();
            _val = successType();
        }
    private:
        explicit Result(successType&& val): _val(val) {
        }

        explicit Result(std::vector<errorType>&& errors): _errors(errors) {
        }
    private:
        std::vector<errorType> _errors;
        successType _val;
    };
}