#include <stdio.h>
#include "compiler.h"

int main(){
	//setSource("word 2 x = 5; word 2 y = 5; x = x + y; x = 5; x = x + 5;");
	setSource("word 2 x = 5; word 2 y = x + 4; y = 5;");
	assembleSource();
	return 0;
}