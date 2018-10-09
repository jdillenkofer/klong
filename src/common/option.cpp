#include "common/option.h"

namespace klong {
    Result<Option, std::string> parseOptions(int argc, char* argv[]) {
        Result<Option, std::string> optionResult;
        Option option;
        int c;
        while ((c = getopt(argc, argv, "hvco:b:d")) != -1)
        {
            switch (c) {
                case 'v':
                    option.verbose = true;
                    break;
                case 'c':
                    option.disableLinking = true;
                    break;
                case 'o':
                    option.useCustomOutputPath = true;
                    option.customOutputPath = std::string(optarg);
                    break;
                case 'b':
                    option.isCustomTarget = true;
                    option.customTarget = std::string(optarg);
                    break;
                case 'd':
                    option.emitDotFile = true;
                    break;
                case 'h':
                default:
                    option.help = true;
                    break;
            }
        }
        // if there is no help option set, we need to check
        if (!option.help) {
            // if there is another argument
            if (optind > (argc - 1)) {
                optionResult.addError("No input file!");
                return optionResult;
            }
            // add the argument as target file to the options
            option.filepath = std::string(argv[optind]);
        }
        optionResult.setSuccess(std::move(option));
        return optionResult;
    }
}