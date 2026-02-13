#include <stdio.h>
#include <stdlib.h>
#include "compiler.h"

int main(int argc, char* argv[]){
	FILE* fptr = fopen(argv[1], "r");
	fseek(fptr, 0, SEEK_END);
    long length = ftell(fptr);
    fseek(fptr, 0, SEEK_SET);
	char *buffer = malloc(length + 1);
	size_t readCount = fread(buffer, 1, length, fptr);
    buffer[readCount] = '\0';
	assembleSource(buffer, 0);
	fclose(fptr);
    free(buffer);
	return 0;
}