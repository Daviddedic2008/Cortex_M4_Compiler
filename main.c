#include <stdio.h>
#include "compiler.h"

int main(){
	setSource("word 1 y = 5; word 1 x = 10; word 1 z = 1 deref (ref x);");
	assembleSource();
	return 0;
}