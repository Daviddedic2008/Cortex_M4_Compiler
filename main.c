#include <stdio.h>
#include "compiler.h"

int main(){
	setSource("word 1 x = 1; word 1 y = 2; word 1 z = y equals x;");
	assembleSource();
	return 0;
}