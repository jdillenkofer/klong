#!/bin/bash

mkdir -p deps
cd deps
if [ ! -f "llvm-7.0.0.src.tar.xz" ]; then
    wget "http://releases.llvm.org/7.0.0/llvm-7.0.0.src.tar.xz"
fi
if [ ! -d "llvm-7.0.0.src" ]; then
    tar -xf "llvm-7.0.0.src.tar.xz"
fi
cd "llvm-7.0.0.src"
mkdir -p build
cd build
cmake -DCMAKE_BUILD_TYPE=Debug ..
sudo make install -j2
cd ../../..
