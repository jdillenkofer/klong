import os
import sys
import subprocess
import platform

DEFAULT_COMPILER_PATH = "build/klong"

def listdir_fullpath(d):
    return [os.path.join(d,f) for f in os.listdir(d)]

def isKlongSource(path):
    return os.path.isfile(path) and path.endswith(".kg")

def isDebugInfo(path):
    return os.path.isfile(path) and path.endswith(".pdb")

def isIncrementalLinkerFile(path):
    return os.path.isfile(path) and path.endswith(".ilk")

def isObjectFile(path):
    return os.path.isfile(path) and path.endswith(".o")

def isExecutable(path):
    return os.path.isfile(path) and path.endswith(".exe")

def isBuildArtifact(path):
    return isDebugInfo(path) or isIncrementalLinkerFile(path) or isObjectFile(path) or isExecutable(path)

def isDir(path):
    return os.path.isdir(path)

def getFileType(baseDir, fileTypeFilter):
    content = listdir_fullpath(baseDir)
    folders = list(filter(isDir, content))
    files = list(filter(fileTypeFilter, content))
    files += [test for folder in folders for test in getFileType(folder, fileTypeFilter)]
    return files

def removeBuildArtifacts(path):
    buildArtifacts = getFileType(path, isBuildArtifact)
    for buildArtifact in buildArtifacts:
        os.remove(buildArtifact)

def compile(path_to_compiler, testfile):
    return subprocess.call([path_to_compiler, "-o", testfile[:-3] + ".exe" , testfile], stdout=subprocess.PIPE)

def run(executable):
    return subprocess.call([os.path.join(".", executable)], stdout=subprocess.PIPE)

def runTests(path_to_compiler, tests, successCallback, errorCallback):
    containsError = False
    for test in tests:
        newPath = os.path.dirname(test)
        os.chdir(newPath)
        test = os.path.basename(test)
        
        compile_result = False
        run_result = False

        try:
            compile_result = compile(path_to_compiler, test) == 0
            if (compile_result):
                executable = test[:-3] + ".exe"
                run_result = run(executable) == 0
        except:
            pass

        test_path = os.path.join(newPath, test)
        if (compile_result and run_result):
            successCallback(test_path)
        else:
            errorCallback(test_path, compile_result, run_result)
            containsError = True        
        os.chdir("..")
    return containsError

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
    
    def successCallback(name): 
        print(".", end="")
        sys.stdout.flush()
    
    def errorCallback(name, compile_result, run_result):
        print(str.format("Test \"{}\" failed! Compile: {} Run: {}", name, compile_result, run_result))
        sys.stdout.flush()
    
    containsError = runTests(path_to_compiler, tests, successCallback, errorCallback)
    
    removeBuildArtifacts(test_path)

    exit(1 if containsError else 0)

if __name__ == "__main__":
    main(len(sys.argv), sys.argv)
