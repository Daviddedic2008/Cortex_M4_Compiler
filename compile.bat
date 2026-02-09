@echo off
SETLOCAL EnableDelayedExpansion

for /F "tokens=1,2 delims=#" %%a in ('"prompt #$H#$E# & echo on & for %%b in (1) do rem"') do set ESC=%%b
set GREEN=!ESC![92m
set RED=!ESC![91m
set YELLOW=!ESC![93m
set BLUE=!ESC![96m
set RESET=!ESC![0m

set "inputFile=%~1"

if exist "build\CortexM4Compiler.exe" (
    echo.
    echo %GREEN%[SUCCESS] Build found.%RESET%
    echo %YELLOW%--- Running ---%RESET%
    echo.
    build\CortexM4Compiler.exe %inputFile%
	if %errorlevel% equ 0 (
		echo %blue%--- Compiled ---%RESET%
	) else (
    echo.
    echo %RED%[ERROR] Compilation failed. Check errors above.%RESET%
    pause
	)
) else (
    echo.
    echo %RED%[ERROR] Compiler binary not found. %RESET%
    pause
)