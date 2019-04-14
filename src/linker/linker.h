#pragma once

#include <vector>
#include <string>

namespace klong {
    bool link(const std::vector<std::string>& objfiles, std::string executableName, bool emitDebugInfo);
}