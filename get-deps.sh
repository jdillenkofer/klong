#!/bin/bash

mkdir -p deps
cd deps
if [ ! -f "clang+llvm-8.0.0-x86_64-linux-gnu-ubuntu-14.04.tar.xz" ]; then
    wget "http://releases.llvm.org/8.0.0/clang+llvm-8.0.0-x86_64-linux-gnu-ubuntu-14.04.tar.xz"
fi
if [ ! -d "clang+llvm-8.0.0-x86_64-linux-gnu-ubuntu-14.04.tar.xz" ]; then
    tar -xf "clang+llvm-8.0.0-x86_64-linux-gnu-ubuntu-14.04.tar.xz"
fi
