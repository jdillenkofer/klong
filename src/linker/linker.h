#pragma once

#include <vector>
#include <string>

namespace klong {
	class Linker {
	public:
		void link(const std::vector<std::string>& objfiles, std::string executableName);
	};
}