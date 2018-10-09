#!/bin/bash

mkdir -p deps
cd deps
wget http://releases.llvm.org/7.0.0/llvm-7.0.0.src.tar.xz
tar -xf llvm-7.0.0.src.tar.xz
cd llvm-7.0.0.src
mkdir -p build
cd build
cmake -DCMAKE_BUILD_TYPE=Debug ..
sudo make install -j
cd ../../..
