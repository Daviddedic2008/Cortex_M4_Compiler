#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>


int flash_with_bootstrap(const unsigned char* stripped_bin, size_t len) {
    const uint32_t SRAM_START  = 0x20000000;
    const uint32_t VECTOR_SIZE = 256;
    const uint32_t CODE_START   = SRAM_START + VECTOR_SIZE; // 0x20000100
    const uint32_t USER_BIN_OFF = 0x30; // 48 bytes for bootstrap + literals
    const uint32_t USER_ADDR    = CODE_START + USER_BIN_OFF; // 0x20000130

    unsigned char bootstrap[] = {
        0x72, 0xB6,             // CPSID i            (Disable interrupts)
        0x03, 0x48,             // LDR R0, [PC, #12]  (Load VTOR_REG)
        0x04, 0x49,             // LDR R1, [PC, #16]  (Load SRAM_BASE)
        0x01, 0x60,             // STR R1, [R0]       (VTOR = 0x20000000)
        
        0x04, 0x48,             // LDR R0, [PC, #16]  (Load USER_ADDR)
        0x01, 0x68,             // LDR R1, [R0]       (Load SP from User Bin)
        0x85, 0x46,             // MOV SP, R1         (Set SP)
        0x41, 0x68,             // LDR R1, [R0, #4]   (Load PC from User Bin)
        0x41, 0xF0, 0x01, 0x01, // ORR R1, R1, #1    (Force Thumb Bit)
        0x08, 0x47,             // BX R1              (Jump!)

        // --- Literal Pool (4-byte aligned) ---
        0x08, 0xED, 0x00, 0xE0, // [0xE000ED08] VTOR Register
        0x00, 0x00, 0x00, 0x20, // [0x20000000] SRAM Base
        0x88, 0xED, 0x00, 0xE0, // [0xE000ED88] CPACR (Optional, for FPU)
        0x30, 0x01, 0x00, 0x20  // [0x20000130] USER_ADDR
    };

    FILE* f = fopen("full_deploy.bin", "wb");
    if (!f) return -1;

    // 1. Create Vector Table
    uint32_t vector_table[64] = {0}; 
    vector_table[0] = 0x10010000;         // Default SP (CCM RAM)
    vector_table[1] = (CODE_START) | 1;   // Reset points to Bootstrap

    // 2. Write everything in order
    fwrite(vector_table, 1, VECTOR_SIZE, f);
    fwrite(bootstrap, 1, sizeof(bootstrap), f);
    
    // Ensure 4-byte alignment for the user binary
    long current_pos = ftell(f);
    while (current_pos % 4 != 0) {
        fputc(0, f);
        current_pos++;
    }

    fwrite(stripped_bin, 1, len, f);
    fclose(f);
    
    char cmd[512];
    sprintf(cmd, "STM32_Programmer_CLI -c port=SWD -w full_deploy.bin 0x%08X -v -g 0x%08X", 
            SRAM_START, SRAM_START);
    
    printf("\nExecuting: %s\n", cmd);
    return system(cmd);
}