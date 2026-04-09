#sample program that blinks the green onboard LED4 on a STM32F407VGT6-DISC board#

1 volatile deref 1073887280 = (1 volatile deref 1073887280) | 8;

1 volatile deref 1073875968 = (1 volatile deref 1073875968) | 16777216;

word delay = 1000000;
while (1) {
    1 volatile deref 1073875988 = (1 volatile deref 1073875988) ^ 4096;

    word i = 0; 
    
    while (i less delay) {
        i += 1;
    }
}