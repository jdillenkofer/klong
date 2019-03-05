#include <cassert>
#include "common/source_range.h"

namespace klong {
    bool SourceRange::valid() const {
        return start.valid() && end.valid();
    }

    uint64_t SourceRange::getStartIndexOfLine(uint64_t currentIndex) const {
        auto code = start.code();
        uint64_t startOfLine = currentIndex;
        if (startOfLine > 0 && code[startOfLine] == '\n') {
            startOfLine--;
        }
        while (startOfLine != 0) {
            if (code[startOfLine] == '\n') {
                startOfLine++;
                break;
            }
            --startOfLine;
        }
        return startOfLine;
    }

    std::string SourceRange::getRelevantSourceText() const {
        assert(start.filename() == end.filename());
        assert(start.absolutepath() == end.absolutepath());

        auto code = start.code();
        if (start.line() == end.line()) {
            uint64_t currentLineStart = getStartIndexOfLine(start.charPos());
            std::stringstream relevantSourceStream;
            std::stringstream strStream(code.substr(currentLineStart));
            std::string line;
            do {
                std::getline(strStream, line);
                relevantSourceStream << line << std::endl;
            } while (!line.empty());
            return relevantSourceStream.str();
        }
        return code.substr(start.charPos(), end.charPos() - start.charPos());
    }
}