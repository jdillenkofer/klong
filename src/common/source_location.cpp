#include "common/source_location.h"

namespace klong {
    void SourceLocation::incLine() {
        _line++;
        _column = 1;
        _charPos++;
    }

    void SourceLocation::incCol() {
        _column++;
        _charPos++;
    }

    std::string SourceLocation::absolutepath() const {
        return _source->absolutepath();
    }

    std::string SourceLocation::filename() const {
        return _source->filename();
    }

    std::string SourceLocation::code() const {
        return _source->code();
    }

    uint32_t SourceLocation::column() const {
        return _column;
    }

    uint32_t SourceLocation::line() const {
        return _line;
    }

    uint64_t SourceLocation::charPos() const {
        return _charPos;
    }

    bool SourceLocation::valid() const {
        return _valid;
    }
}