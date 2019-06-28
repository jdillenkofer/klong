cmake_minimum_required(VERSION 3.9)
project(klong)

find_package(LLVM 8.0.0 REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

set(EXECUTABLE_NAME
    klong
)

if(("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU") OR ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "CLANG"))
  set(CMAKE_CXX_FLAGS
    "${CMAKE_CXX_FLAGS} \
     -g \
     -Wall \
     -Wextra \
     -Wconversion \
     -Wunused \
     -Wunused-function \
     -Wunused-label \
     -Wunused-macros \
     -Wunused-parameter \
     -Wunused-value \
     -Wunused-variable \
     -Wcast-align \
     -Wcast-qual \
     -Wmissing-declarations \
     -Wredundant-decls \
     -Wpointer-arith \
     -Wshadow \
     -Wwrite-strings \
     -Wswitch \
     -Wmissing-field-initializers \
     -fno-common \
     -pedantic \
     -std=c++17")

	set(LINK_FLAGS
	 "-lstdc++fs")
endif()

if(MSVC)
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /std:c++17")
endif(MSVC)

include_directories(src/)
include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

set(SOURCE_FILES
    src/main.cpp
    src/compiler.cpp
    src/common/getopt.c
    src/common/option.cpp
    src/common/source_range.cpp
    src/common/source_location.cpp
    src/common/compilation_result.cpp
    src/ast/string_helper.cpp
    src/ast/type.cpp
    src/lexer/lexer.cpp
    src/parser/parser.cpp
    src/resolver/resolver.cpp
    src/resolver/resolve_visitor.cpp
    src/typechecker/typechecker.cpp
    src/typechecker/type_check_visitor.cpp
    src/codegen/c/c_emitter.cpp
    src/codegen/c/c_emit_visitor.cpp
    src/codegen/llvm/llvm_emitter.cpp
    src/codegen/llvm/llvm_emit_visitor.cpp
    src/codegen/llvm/llvm_type_emit_visitor.cpp
	src/codegen/llvm/llvm_debug_scope_manager.cpp
	src/codegen/llvm/llvm_debug_type_emit_visitor.cpp
	src/linker/linker.cpp
    src/graphviz/dotfile_visitor.cpp
    src/graphviz/dotfile_emitter.cpp
	src/common/compilation_session.cpp)

set(HEADER_FILES
    src/compiler.h
    src/common/defer.h
    src/common/option.h
    src/common/getopt.h
    src/common/result.h
    src/common/source_range.h
	src/common/source_file.h
    src/common/source_location.h
    src/common/compilation_result.h
    src/common/compilation_error.h
    src/common/compilation_warning.h
    src/ast/expr.h
    src/ast/stmt.h
	src/ast/module.h
    src/ast/type.h
    src/ast/visitor.h
    src/ast/string_helper.h
    src/lexer/token.h
    src/lexer/lexer.h
    src/lexer/lexer_memento.h
    src/parser/parser_memento.h
	src/parser/parser.h
    src/resolver/resolver.h
	src/resolver/resolve_visitor.h
	src/typechecker/typechecker.h
    src/typechecker/type_check_visitor.h
    src/codegen/c/c_emitter.h
    src/codegen/c/c_emit_visitor.h
    src/codegen/llvm/llvm_emit_visitor.h
    src/codegen/llvm/llvm_emitter.h
    src/codegen/llvm/llvm_type_emit_visitor.h
	src/codegen/llvm/llvm_debug_scope_manager.h
	src/codegen/llvm/llvm_debug_type_emit_visitor.h
	src/linker/microsoft_craziness.h
	src/linker/linker.h
    src/graphviz/dotfile_visitor.h
	src/graphviz/dotfile_emitter.h
    src/common/compilation_session.h
	src/common/symbol_info.h)

# Find the libraries that correspond to the LLVM components
# that we wish to use
llvm_map_components_to_libnames(llvm_libs 
	FuzzMutate
	XCoreDisassembler
	XCoreCodeGen
	XCoreDesc
	XCoreInfo
	XCoreAsmPrinter
	SystemZDisassembler
	SystemZCodeGen
	SystemZAsmParser
	SystemZDesc
	SystemZInfo
	SystemZAsmPrinter
	SparcDisassembler
	SparcCodeGen
	SparcAsmParser
	SparcDesc
	SparcInfo
	SparcAsmPrinter
	PowerPCDisassembler
	PowerPCCodeGen
	PowerPCAsmParser
	PowerPCDesc
	PowerPCInfo
	PowerPCAsmPrinter
	NVPTXCodeGen
	NVPTXDesc
	NVPTXInfo
	NVPTXAsmPrinter
	MSP430CodeGen
	MSP430Desc
	MSP430Info
	MSP430AsmPrinter
	MipsDisassembler
	MipsCodeGen
	MipsAsmParser
	MipsDesc
	MipsInfo
	MipsAsmPrinter
	LanaiDisassembler
	LanaiCodeGen
	LanaiAsmParser
	LanaiDesc
	LanaiAsmPrinter
	LanaiInfo
	HexagonDisassembler
	HexagonCodeGen
	HexagonAsmParser
	HexagonDesc
	HexagonInfo
	BPFDisassembler
	BPFCodeGen
	BPFAsmParser
	BPFDesc
	BPFInfo
	BPFAsmPrinter
	ARMDisassembler
	ARMCodeGen
	ARMAsmParser
	ARMDesc
	ARMInfo
	ARMAsmPrinter
	ARMUtils
	AMDGPUDisassembler
	AMDGPUCodeGen
	AMDGPUAsmParser
	AMDGPUDesc
	AMDGPUInfo
	AMDGPUAsmPrinter
	AMDGPUUtils
	AArch64Disassembler
	AArch64CodeGen
	AArch64AsmParser
	AArch64Desc
	AArch64Info
	AArch64AsmPrinter
	AArch64Utils
    msp430asmparser
    msp430asmprinter
    msp430codegen
    msp430desc
    msp430disassembler
    msp430info
    webassemblyasmparser
    webassemblyasmprinter
    webassemblycodegen
    webassemblydesc
    webassemblydisassembler
    webassemblyinfo
	Symbolize
	DebugInfoPDB
	DebugInfoDWARF
	TableGen
	LineEditor
	OrcJIT
	MIRParser
	DlltoolDriver
	X86Disassembler
	X86AsmParser
	X86CodeGen
	GlobalISel
	SelectionDAG
	AsmPrinter
	X86Desc
	MCDisassembler
	X86Info
	X86AsmPrinter
	X86Utils
	MCJIT
	Coverage
	LibDriver
	Option
	Coroutines
	ObjectYAML
	WindowsManifest
	Interpreter
	ExecutionEngine
	RuntimeDyld
	Passes
	ObjCARCOpts
	ipo
	Instrumentation
	Vectorize
	Linker
	IRReader
	AsmParser
	CodeGen
	Target
	ScalarOpts
	InstCombine
	BitWriter
	AggressiveInstCombine
	TransformUtils
	Analysis
	ProfileData
	Object
	MCParser
	MC
	DebugInfoCodeView
	DebugInfoMSF
	BitReader
	Core
	BinaryFormat
	Support
	Demangle
)

add_executable(${EXECUTABLE_NAME} ${SOURCE_FILES} ${HEADER_FILES})

# Link against LLVM libraries
target_link_libraries(${EXECUTABLE_NAME} ${llvm_libs} ${LINK_FLAGS})
