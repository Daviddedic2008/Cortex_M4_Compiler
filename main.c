#include <stdio.h>
#include "compiler.h"

int main(){
	setSource("word 1 x = 5;");
	assembleSource();
	return 0;
}