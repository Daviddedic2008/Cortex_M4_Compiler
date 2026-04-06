#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

/*
 * Refactored Bootloader for SRAM execution.
 * Layout:
 * 0x20000000: Vector Table (256 bytes)
 * 0x20000100: Bootstrap Code (HSE Init + VTOR config)
 * 0x20000100 + sizeof(bootstrap): User Binary
 */

int flash_with_bootstrap(const unsigned char* stripped_bin, size_t len) {
    const uint32_t SRAM_START = 0x20000000;
    const uint32_t SRAM_END   = 0x20020000; // 128KB SRAM limit for F401
    const uint32_t CODE_START = 0x20000100; // Offset for bootstrap entry

    // Bootstrap: Enables HSE, sets VTOR to 0x20000000, then drops into user code.
    // Ensure literal pool offsets match the instructions.
    unsigned char bootstrap[] = {
        0x04, 0x48, 0x05, 0x49, 0x01, 0x60, // LDR R0, [VTOR_ADDR]; LDR R1, [SRAM_ADDR]; STR R1, [R0]
        0x05, 0x48, 0x01, 0x68,             // LDR R0, [CPACR]; LDR R1, [R0]
        0x41, 0xF4, 0x70, 0x01, 0x01, 0x60, // ORR R1, #0xF00000 (Enable FPU); STR R1, [R0]
        0xBF, 0xF3, 0x4F, 0x8F,             // DSB
        0xBF, 0xF3, 0x6F, 0x8F,             // ISB
        // Literal Pool (Placed after code to avoid execution)
        0x08, 0xED, 0x00, 0xE0,             // [VTOR Address: 0xE000ED08]
        0x00, 0x00, 0x00, 0x20,             // [SRAM Address: 0x20000000]
        0x88, 0xED, 0x00, 0xE0              // [CPACR Address: 0xE000ED88]
    };

    FILE* f = fopen("full_deploy.bin", "wb");
    if (!f) return -1;

    // 1. Generate 256-byte Aligned Vector Table
    uint32_t vector_table[64] = {0}; 
    vector_table[0] = SRAM_END;            // Initial SP
    vector_table[1] = (CODE_START) | 1;    // Reset Vector (Points to bootstrap)
    
    // 2. Write Table (0x00 to 0xFF)
    fwrite(vector_table, 1, 256, f);

    // 3. Write Bootstrap (Starts at 0x100)
    fwrite(bootstrap, 1, sizeof(bootstrap), f);

    // 4. Write User Program
    fwrite(stripped_bin, 1, len, f);
    fclose(f);

    // Command Logic:
    // -w: Writes the file to SRAM.
    // -g: Tells the built-in ST bootloader to jump to 0x20000000.
    // The hardware will then read vector_table[0] as SP and vector_table[1] as PC.
    char cmd[512];
    sprintf(cmd, "STM32_Programmer_CLI -c port=SWD -w full_deploy.bin 0x%08X -v -g 0x%08X", SRAM_START, SRAM_START);
    
    printf("\n\nExecuting: %s\n", cmd);
    return system(cmd);
}