import os
import sys
import subprocess
import platform

DEFAULT_COMPILER_PATH = "build/klong"
DEFAULT_TEST_PATH = "test"

def listdir_fullpath(d):
    return [os.path.join(d,f) for f in os.listdir(d)]

def isKlongSource(path):
    return os.path.isfile(path) and path.endswith(".kg")

def isObjectFile(path):
    return os.path.isfile(path) and path.endswith(".o")

def isExecutable(path):
    return os.path.isfile(path) and path.endswith(".exe")

def isBuildArtefact(path):
    return isObjectFile(path) or isExecutable(path)

def isDir(path):
    return os.path.isdir(path)

def getFileType(baseDir, fileTypeFilter):
    content = listdir_fullpath(baseDir)
    folders = list(filter(isDir, content))
    files = list(filter(fileTypeFilter, content))
    files += [test for folder in folders for test in getFileType(folder, fileTypeFilter)]
    return files

def removeBuildArtifacts(pwd):
    buildArtefacts = getFileType(pwd, isBuildArtefact)
    for buildArtefact in buildArtefacts:
        os.remove(buildArtefact)

def compile(path_to_compiler, testfile):
    return subprocess.call([path_to_compiler, testfile], stdout=subprocess.PIPE)

def link(objfile, executable):
    system = platform.system()
    if  (system == "Windows"):
        result = subprocess.call([
            "link.exe", 
            "/NOLOGO",
            "/SUBSYSTEM:CONSOLE", 
            "/MACHINE:x64", 
            "/DEFAULTLIB:libcmt", 
            objfile, 
            "/OUT:" + executable,
            "/LIBPATH:C:/Program Files (x86)/Microsoft Visual Studio/2017/Community/VC/Tools/MSVC/14.15.26726/lib/x64",
            "/LIBPATH:C:/Program Files (x86)/Windows Kits/10/Lib/10.0.17134.0/um/x64", 
            "/LIBPATH:C:/Program Files (x86)/Windows Kits/10/Lib/10.0.17134.0/ucrt/x64"
        ])
    else:
        result = subprocess.call([
            # use ld here instead of gcc
            "gcc",
            "-o",
            executable,
            objfile
        ])
    return result
        

def run(executable):
    return subprocess.call([os.path.join(".", executable)])

def runTests(pwd, path_to_compiler, tests):
    for test in tests:
        newPath = os.path.dirname(test)
        os.chdir(newPath)
        test = os.path.basename(test)
        test_filename, _ = os.path.splitext(test)
        compile(path_to_compiler, test)
        link(test_filename + ".o", test_filename + ".exe")
        run(test_filename + ".exe")
        os.chdir(pwd)

def main(argc, argv):
    pwd = os.getcwd()
    path_to_compiler = DEFAULT_COMPILER_PATH
    
    if (argc == 2):
        path_to_compiler = argv[1]
    
    if (not os.path.isabs(path_to_compiler)):
        path_to_compiler = os.path.join(pwd, path_to_compiler)

    tests = getFileType(DEFAULT_TEST_PATH, isKlongSource)
    runTests(pwd, path_to_compiler, tests)
    removeBuildArtifacts(DEFAULT_TEST_PATH)

if __name__ == "__main__":
    main(len(sys.argv), sys.argv)