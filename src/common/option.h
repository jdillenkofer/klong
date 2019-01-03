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
		bool emitDebugInfo = false;
		bool emitDwarf = true;
        bool useCustomOutputPath = false;
        std::string customOutputPath;
        bool isCustomTarget = false;
        std::string customTarget;
		bool printIR = false;
        bool emitDotFile = false;
        bool emitAssemblyFile = false;
        std::string filepath;
    };

    Result<Option, std::string> parseOptions(int argc, char* argv[]);
}