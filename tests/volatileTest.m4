#program to blink green onboard LED (bound to pin d12 i think?) on the STM32F407VGT6-DISC board#

1 volatile deref 1073887280 = (1 volatile deref 1073887280) | 8;

1 volatile deref 1073875968 = (1 volatile deref 1073875968) | 16777216;

while (1) {
    1 volatile deref 1073875988 = (1 volatile deref 1073875988) ^ 4096;

    word i = 0; 
    word max = 10000000;
    while (i less max) {
        i += 1;
    }
}