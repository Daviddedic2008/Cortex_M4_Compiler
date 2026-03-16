# fibonacci test #
argument fibIdx; # input is the fibonacci number we need to calculate #

word n1 = 0; word n2 = 1;
while(fibIdx greater equals 0){
	word tmp = n1 + n2;
	n1 = n2; n2 = tmp;
	fibIdx -= 1;
}

return n2; # script will return the number #
