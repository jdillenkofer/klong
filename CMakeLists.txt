cmake_minimum_required(VERSION 3.11)
project(klong)

find_package(LLVM REQUIRED CONFIG)

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
endif()

if(MSVC)
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /std:c++17")
endif(MSVC)

include_directories(src/)
include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

set(SOURCE_FILES
    src/main.cpp
    src/lexer/lexer.cpp
    src/parser/parser.cpp
    src/parser/resolver.cpp
    src/parser/type_checker.cpp
    src/codegen/LLVMEmitter.cpp)

set(HEADER_FILES
    src/common/source_file.h
    src/common/source_location.h
    src/lexer/token.h
    src/lexer/lexer.h
    src/lexer/ilexer.h
    src/parser/expr.h
    src/parser/stmt.h
    src/parser/visitor.h
    src/parser/parser.h
    src/parser/iparser.h
    src/parser/type.h
    src/common/source_range.h
    src/parser/resolver.h
    src/parser/type_checker.h
    src/codegen/LLVMEmitter.h)

# Find the libraries that correspond to the LLVM components
# that we wish to use
llvm_map_components_to_libnames(llvm_libs support core all)

add_executable(${EXECUTABLE_NAME} ${SOURCE_FILES} ${HEADER_FILES})

# Link against LLVM libraries
target_link_libraries(${EXECUTABLE_NAME} ${llvm_libs})