/*
POSIX getopt for Windows

AT&T Public License

Code given out at the 1985 UNIFORUM conference in Dallas.
*/
#pragma once

#ifdef __unix__
// Use POSIX getopt, if it exists
#include <unistd.h>
#else

extern int opterr;
extern int optind;
extern int optopt;
extern char *optarg;
extern int getopt(int argc, char** argv, const char* opts);

#endif  /* __unix__ */