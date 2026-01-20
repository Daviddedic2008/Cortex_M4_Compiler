# Toy Compiler for Cortex-M4

A lightweight C-based compiler targeting the ARM Cortex-M4 architecture. This project is designed to be efficient enough to eventually run as a self-hosted compiler directly on Cortex-M4 hardware.

## Current Status
* **Target:** ARM Cortex-M4.
* **Host Build:** Windows x64 using GNU GCC.
* **Feature Goal:** Self-hosting capabilities and direct flashing to hardware.

## Project Files
* `main.c`
* `compiler.c` : Main file
* `CMakeLists.txt`: Build system configuration.
* `bld.bat`: Standard build and run script.
* `remake.bat`: Clean and cache refresh script.

## Usage

### Build and Run
To compile the project and execute the compiler in one step:
```cmd
bld