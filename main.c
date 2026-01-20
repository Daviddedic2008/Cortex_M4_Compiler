#include <stdio.h>
#include "compiler.h"

int main(){
	setSource("word 4 x = (5 + 7) + 3;");
	assembleSource();
	return 0;
}