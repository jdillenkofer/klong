#pragma once

#include <string>

#include "common/result.h"
extern "C" {
#include "common/getopt.h"
}

namespace klong {
    struct Option {
        bool help = false;
        bool verbose = false;
        bool disableLinking = false;
        bool useCustomOutputPath = false;
        std::string customOutputPath;
        bool isCustomTarget = false;
        std::string customTarget;
        bool emitDotFile = false;
        std::string filepath;
    };

    Result<Option, std::string> parseOptions(int argc, char* argv[]);
}