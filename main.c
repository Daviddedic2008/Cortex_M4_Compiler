#include <stdio.h>
#include "compiler.h"

int main(){
	setSource("1 deref 100 = 5;");
	initializeVirtualRegs();
	assembleSource();
	return 0;
}