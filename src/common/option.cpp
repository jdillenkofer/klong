#include "common/option.h"

namespace klong {
    Result<Option, std::string> parseOptions(int argc, char* argv[]) {
        Result<Option, std::string> optionResult;
        Option option;
        int c;
        while ((c = getopt(argc, argv, "hvcgdeso:b:ip")) != -1)
        {
            switch (c) {
                case 'v':
                    option.verbose = true;
                    break;
                case 'c':
                    option.disableLinking = true;
                    break;
				case 'g':
					option.emitDebugInfo = true;
					break;
				case 'd':
					option.emitDwarf = false;
					break;
                case 'e':
                    option.useCBackend = true;
                    break;
                case 's':
                    option.emitAssemblyFile = true;
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
				case 'i':
					option.printIR = true;
					break;
                case 'p':
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
                optionResult.setError("No input file!");
                return optionResult;
            }
            // add the argument as target file to the options
            option.filepath = std::string(argv[optind]);
			optind++;
			if (optind != argc) {
				optionResult.setError("Invalid argument after target file.");
				return optionResult;
			}
        }
        optionResult.setSuccess(std::move(option));
        return optionResult;
    }
}