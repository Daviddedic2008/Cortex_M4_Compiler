# fibonacci test #
argument x; # input is the fibonacci number we need to calculate #

word n1 = 0; word n2 = 1;
while(x greater equals 0){
	word tmp = n1 + n2;
	n1 = n2; n2 = tmp;
}

return n2; # script will return the number #
