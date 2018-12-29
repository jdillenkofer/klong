#include "linker.h"

#include <string>
#ifdef WIN32
#include "microsoft_craziness.h"

#include <locale>
#include <sstream>
#include <string>
#include <stdlib.h>

std::string ToNarrow(const wchar_t *s, char dfault = '?', const std::locale& loc = std::locale()) {
	std::ostringstream stm;

	while (*s != L'\0') {
		stm << std::use_facet< std::ctype<wchar_t> >(loc).narrow(*s++, dfault);
	}
	return stm.str();
}

#endif

namespace klong {
	void Linker::link(const std::vector<std::string>& objfiles, std::string executableName, bool emitDebugInfo) {
		std::string command = "";
#ifdef WIN32
		Find_Result result = find_visual_studio_and_windows_sdk();
		std::string vsExePath = ToNarrow(result.vs_exe_path);
		std::string vslibPath = ToNarrow(result.vs_library_path);
		std::string umPath = ToNarrow(result.windows_sdk_um_library_path);
		std::string ucrtPath = ToNarrow(result.windows_sdk_ucrt_library_path);
		free_resources(&result);

		command += "\"";
		command += "\"" + vsExePath + "\\link.exe\" ";
		command += "/NOLOGO ";
		if (emitDebugInfo) {
			command += "/DEBUG ";
		}
		// command += "/SUBSYSTEM:CONSOLE ";
		command += "/MACHINE:x64 ";
		command += "/DEFAULTLIB:libcmt ";
		for (auto& objfile : objfiles) {
			command += "\"" + objfile + "\" ";
		}
		command += "/OUT:\"" + executableName + "\" ";
		command += "/LIBPATH:\"" + vslibPath + "\" ";
		command += "/LIBPATH:\"" + umPath + "\" ";
		command += "/LIBPATH:\"" + ucrtPath + "\"";
		command += "\"";
#elif defined(__unix__) || defined(__unix)
		// FOR APPLE USE THESE: || (defined(__APPLE__) && defined(__MACH__))
		command += "gcc ";
        if (emitDebugInfo) {
            command += "-g ";
        }
        command += "-o \"" + executableName + "\" ";
		for (auto& objfile : objfiles) {
			command += "\"" + objfile + "\" ";
		}
#endif
		system(command.c_str());
	}
}
