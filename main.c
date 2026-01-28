#include <stdio.h>
#include "compiler.h"

int main(){
	setSource("word 1 x = 5; word 1 y = x + 5; word 1 z = x + 5;");
	initializeVirtualRegs();
	assembleSource();
	return 0;
}