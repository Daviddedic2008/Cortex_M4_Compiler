#include <stdio.h>
#include "compiler.h"

int main(){
	setSource("word 1 y = 1 deref 100;");
	assembleSource();
	return 0;
}