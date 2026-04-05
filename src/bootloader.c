#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

/*
Gemini-generated barebones bootloader script, uses stm32 programmer to load the compiler's outputted "stripped" binary
along with the necessary startup machine code to use faster oscillator crystal.
Pretty sure it does not manage the vector interrupt table address/handler addresses properly.
It flashes to SRAM to stop flash wear for debugging
*/

int flash_with_bootstrap(const unsigned char* stripped_bin, size_t len) {
    const uint32_t SRAM_START = 0x20000000;
    const uint32_t SRAM_END   = 0x20020000;
    
    unsigned char bootstrap[] = {
        0x04, 0x48, 0x05, 0x49, 0x01, 0x60, 0x05, 0x48, 
        0x01, 0x68, 0x41, 0xF4, 0x70, 0x01, 0x01, 0x60,
        0xBF, 0xF3, 0x4F, 0x8F, 0xBF, 0xF3, 0x6F, 0x8F,
        0x00, 0x00, 0x00, 0x00, // Placeholder for Literal Pool
        0x08, 0xED, 0x00, 0xE0, // VTOR Address: 0xE000ED08
        0x00, 0x00, 0x00, 0x20, // SRAM Address: 0x20000000
        0x88, 0xED, 0x00, 0xE0  // CPACR Address: 0xE000ED88
    };
    FILE* f = fopen("full_deploy.bin", "wb");
    if (!f) return -1;

    uint32_t header[2] = { SRAM_END, (SRAM_START + 8) | 1 };
	uint32_t execute_addr = (0x20000000 + 8) | 1;
    
    fwrite(header, sizeof(uint32_t), 2, f);
    fwrite(bootstrap, 1, sizeof(bootstrap), f);
    fwrite(stripped_bin, 1, len, f);
    fclose(f);
	char cmd[256];
	sprintf(cmd, "STM32_Programmer_CLI -c port=SWD -w full_deploy.bin 0x20000000 -v -g 0x%08X", execute_addr);
    return system(cmd);
}