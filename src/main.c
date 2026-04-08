#include <stdio.h>
#include <stdlib.h>
#include "compiler.h"
#include "bootloader.h"

int main(int argc, char* argv[]){
	FILE* fptr = fopen(argv[1], "r");
	fseek(fptr, 0, SEEK_END);
    long length = ftell(fptr);
    fseek(fptr, 0, SEEK_SET);
	char *buffer = malloc(length + 1);
	size_t readCount = fread(buffer, 1, length, fptr);
    buffer[readCount] = '\0';
	fclose(fptr);
	uint8_t bufferArr[2048] = {0}; // output binary buffer
	const uint8_t errCd = assembleSource(buffer, bufferArr);
	flash_with_bootstrap(bufferArr, 2048);
    free(buffer);
	return errCd;
}