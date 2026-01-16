#include <stdio.h>
#include "compiler.h"

int main(){
	setSource("word x = (5 + 7) * 3;");
	nextToken();
	nextToken();
	nextToken();
	nextToken();
	nextToken();
	nextToken();
	nextToken();
	nextToken();
	nextToken();
	nextToken();
	nextToken();
	return 0;
}