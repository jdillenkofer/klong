import os
import sys
import subprocess
import platform

DEFAULT_COMPILER_PATH = "build/klong"

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

def removeBuildArtifacts(path):
    buildArtefacts = getFileType(path, isBuildArtefact)
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

def runTests(path_to_compiler, tests):
    test_results = []
    for test in tests:
        newPath = os.path.dirname(test)
        os.chdir(newPath)
        test = os.path.basename(test)
        test_filename, _ = os.path.splitext(test)
        
        compile_result = False
        link_result = False
        run_result = False

        compile_result = compile(path_to_compiler, test) == 0
        if (compile_result):
            link_result = link(test_filename + ".o", test_filename + ".exe") == 0
        if (link_result):
            run_result = run(test_filename + ".exe") == 0

        if (compile_result and link_result and run_result):
            test_results.append((".", None))
        else:
            test_results.append((None, str.format("Test \"{}\" failed! Compile: {} Link: {} Run: {}", os.path.join(newPath, test), compile_result, link_result, run_result)))        
        os.chdir("..")
    return test_results

def main(argc, argv):
    test_path = os.path.dirname(argv[0])
    path_to_compiler = DEFAULT_COMPILER_PATH
        
    if (argc == 2):
        path_to_compiler = argv[1]
        if (not os.path.isabs(path_to_compiler)):
            path_to_compiler = os.path.join(os.getcwd(), path_to_compiler)
    else:
        project_root = os.path.abspath(os.path.join(test_path, os.pardir))
        path_to_compiler = os.path.join(project_root, path_to_compiler)
    
    tests = getFileType(test_path, isKlongSource)
    test_results = runTests(path_to_compiler, tests)
    
    containsError = False
    for test_result in test_results:
        if (test_result[0] != None):
            print(test_result[0], end="")
        else:
            containsError = True
            print(test_result[1])
    
    removeBuildArtifacts(test_path)

    exit(1 if containsError else 0)

if __name__ == "__main__":
    main(len(sys.argv), sys.argv)