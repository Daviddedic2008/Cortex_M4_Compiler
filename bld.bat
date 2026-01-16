@echo off
SETLOCAL EnableDelayedExpansion

for /F "tokens=1,2 delims=#" %%a in ('"prompt #$H#$E# & echo on & for %%b in (1) do rem"') do set ESC=%%b
set GREEN=!ESC![92m
set RED=!ESC![91m
set YELLOW=!ESC![93m
set BLUE=!ESC![96m
set RESET=!ESC![0m

echo %YELLOW%--- Compiling ---%RESET%
cmake --build --preset x64-debug

if %errorlevel% equ 0 (
    echo.
    echo %GREEN%[SUCCESS] Build finished successfully.%RESET%
    echo %YELLOW%--- Running ---%RESET%
    echo.
    build\CortexM4Compiler.exe
) else (
    echo.
    echo %RED%[ERROR] Build failed. Check the errors above.%RESET%
    pause
)