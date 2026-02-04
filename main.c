#include <stdio.h>
#include "compiler.h"

int main(){
	setSource("word 1 x = 1; x = x + 5; word 1 y = x + 4;");
	assembleSource();
	return 0;
}