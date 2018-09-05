cmake_minimum_required(VERSION 3.11)
project(klong)

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
     -std=c++2a")
endif()

include_directories(src/)

set(SOURCE_FILES
    src/main.cpp
    src/lexer/lexer.cpp
    src/parser/parser.cpp
)

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
)

add_executable(${EXECUTABLE_NAME} ${SOURCE_FILES} ${HEADER_FILES})