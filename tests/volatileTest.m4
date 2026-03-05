# blink some LED or something #
while 1{
	1 volatile deref 101010 = 1; # placeholder addr for now #
	word idx = 0;
	while idx less 100000{idx += 1; idx -= 1; idx += 1;} # delay #
	1 volatile deref 101010 = 0; # placeholder addr for now #
}