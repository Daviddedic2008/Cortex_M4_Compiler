word x = 4;# manual register management test #
word x = 0;
free x;
flush word x2 = 1;
word y = x2;
free x2;
word z = 5; 

# register reconciliation optimizations test #
word 64 arr;
word idx = 0;
while(idx less word 64){
	1 in arr (idx) = idx;
	idx += 4;
}