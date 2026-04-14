#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

// stupidest upload script ever written

int flash_with_bootstrap(uint8_t* binArray, uint32_t arrayLength) {
	uint32_t loadAddr = 0x08000000, stackPtr = 0x20020000;
    const char* tmp = "payload.bin";

    FILE* fp = fopen(tmp, "wb");
    if (!fp) return -1;
    fwrite(binArray, 1, arrayLength, fp);
    fclose(fp);

    char cmd[1024];
	snprintf(cmd, sizeof(cmd), 
    "STM32_Programmer_CLI -c port=SWD freq=4000 mode=UR -rst -w %s 0x20000000 "
    "-coreReg msp=0x10000001 "
    "-coreReg pc=0x20000001 "
    "-run", 
    tmp);


    printf("Executing manual init sequence...\n");
    int result = system(cmd);
    
    remove(tmp);
    return result;
}