@echo off
SETLOCAL EnableDelayedExpansion

:: Define the Escape character and Colors
for /F "tokens=1,2 delims=#" %%a in ('"prompt #$H#$E# & echo on & for %%b in (1) do rem"') do set ESC=%%b
set GREEN=!ESC![92m
set RED=!ESC![91m
set YELLOW=!ESC![93m
set BLUE=!ESC![96m
set RESET=!ESC![0m

echo %YELLOW%--- Deep Cleaning Project ---%RESET%

if exist build (
    echo %BLUE%[INFO] Removing old build folder...%RESET%
    rd /s /q build
) else (
	echo %RED% No build folder exists! %RESET%
)

echo %YELLOW%--- Regenerating CMake Cache ---%RESET%
cmake --preset x64-gnu-debug

if %errorlevel% neq 0 (
    echo.
    echo %RED%[ERROR] Configuration failed!%RESET%
    pause
    exit /b %errorlevel%
)

echo.
echo %YELLOW%--- Performing Initial Build ---%RESET%
cmake --build --preset x64-debug

if %errorlevel% equ 0 (
    echo.
    echo %GREEN%[SUCCESS] Project remade from scratch!%RESET%
    echo %BLUE%You can now use your regular bld.bat%RESET%
) else (
    echo.
    echo %RED%[ERROR] Build failed after reset.%RESET%
    pause
)