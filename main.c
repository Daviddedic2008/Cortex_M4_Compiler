#include <stdio.h>
#include <stdlib.h>
#include "compiler.h"

int main(int argc, char* argv[]){
	//setSource("word 2 x = 5; word 2 y = 5; x = x + y; x = 5; x = x + 5;");
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