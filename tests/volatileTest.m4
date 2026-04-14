# 1. Enable Clock for Port A (bit 0) and Port D (bit 3) #
1 volatile deref 1073887280 = (1 volatile deref 1073887280) | 9;

# 2. GPIOA_MODER: PA0 to INPUT #
1 volatile deref 1073872896 = (1 volatile deref 1073872896) & 4294967292;

# 3. GPIOA_PUPDR: No Pull #
1 volatile deref 1073872908 = (1 volatile deref 1073872908) & 4294967292;

# 4. GPIOD_MODER: Set PD12 to Output #
1 volatile deref 1073875968 = (1 volatile deref 1073875968) | 16777216;

# Setup Delay Array #
# Since word 1 = 4, setupIdx += word 1 moves 4 bytes at a time #
word 4 delayArr;
word setupIdx = 0;
while(setupIdx less 16){
    1 in delayArr (setupIdx) = (setupIdx + 4) * 50000;
    setupIdx += word 1;
} free setupIdx;

word idx = 0;

while (1) {
    # Toggle LED #
    1 volatile deref 1073875988 = (1 volatile deref 1073875988) ^ 4096;
    
    # Read Button #
    word status = 1 volatile deref 1073872912;
    
    if(status & 1){
        # Move to next word (4 bytes) #
        idx += word 1;
        
        # If we hit the end of the 16-byte array, reset #
        if(idx greater 12){
            idx = 0;
        }
        
        # RELEASE LOOP #
        # Re-polling inside the IF to ensure we don't move until released #
        word pressCheck = 1;
        while(pressCheck & 1){
            pressCheck = 1 volatile deref 1073872912;
        }
    }
    
    # Get delay and wait #
    word delay = 1 in delayArr (idx);
    word i = 0;
    while(i less delay){
        i += 1;
    }
}