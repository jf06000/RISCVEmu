cmake_minimum_required(VERSION 3.20)

# Project declaration
project(RISCVEMU)

# Set the C++ standard to use
set(CMAKE_CXX_STANDARD 17)
message("CMAKE_PREFIX_PATH -> " "${CMAKE_FIND_PACKAGE_REDIRECTS_DIR}")

include_directories(../src)

# Exe declaration
add_executable(RISCVEMU
    ../src/memory.cpp
    ../src/RISCV.cpp
    ../src/RISCVEmu.cpp
)
