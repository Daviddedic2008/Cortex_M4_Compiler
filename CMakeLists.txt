cmake_minimum_required(VERSION 3.10)

# Set project name and restrict to GNU C
project(CortexM4Compiler LANGUAGES C)

# Ensure strictly GNU C (C11 is standard for modern compilers)
set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)

# Useful for compiler dev to catch alignment or syntax bugs early
add_compile_options(-Wall -Wextra)

add_executable(CortexM4Compiler
    main.c 
	compiler.c
)
target_compile_options(${PROJECT_NAME} PRIVATE -O2)
target_link_options(${PROJECT_NAME} PRIVATE -s)
target_link_options(${PROJECT_NAME} PRIVATE -Wl,--stack,16384)
target_compile_options(CortexM4Compiler PRIVATE -w)
