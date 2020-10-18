#pragma once

#include "array.h"
#include <string>

namespace klong {
    bool link(const Array<std::string>& objfiles, std::string executableName, bool emitDebugInfo);
}