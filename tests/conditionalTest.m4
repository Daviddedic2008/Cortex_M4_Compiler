word idx = 0;
word 64 arr; word result = 0;
while idx less word 64{
	if(1 in arr (idx) equals 0){result = 1; break;}
	else{idx += 4;}
} 

idx = -4; result = 0;
while(idx less word 64){
	idx += 4	;
	if(1 in arr (idx) equals 0){continue;}
	result = 1;
}