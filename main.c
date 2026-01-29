#include <stdio.h>
#include "compiler.h"

int main(){
	setSource("word 1 x = 4; x = x + 5; x = 5; word 1 y = 5;");
	assembleSource();
	return 0;
}