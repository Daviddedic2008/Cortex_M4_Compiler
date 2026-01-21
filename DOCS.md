# Language Documentation

Lightweight language optimized for 32 bit arithmetic and data structures

## Syntax

### Variable Declaration
Variables are statically sized(not really typed). They can be sized to multiples of 32 bit words.
Each declaration automatically allocates stack space for said variable.
Variables that are one word can live in registers, while bigger variables are treated as write-through.
Therefore, use one word variables for values that must stay in registers to be accessed easily.
```rust
// word [count] name = value;
// [count] must be a constant
word 1 variable;	// simply declares it, no assignment
word 1 x = 10;	// a single 32-bit integer and assigns 10 to it
word 4 buffer = 0;	// reserves 16 bytes (4 words) on the stack and zeros the variable
```

### Pointer Usage
Pointers are simply 1 word variables. 
This compiler is made for 32 bit systems, so therefore only accepts 32 bit pointers.

#### deref
To get data stored at memory locations, use the deref operator.
Pointer 'casting' is done at dereferencing, and therefore the operator takes a dereference size too.
The size being dereferenced is static. you cannot dereference a dynamic amount of words.
```rust
// [size] deref [address];
// [size] must be a constant. [address] can be a variable or constant
1 deref 100;	// reads one word from absolute memory location 100
word 1 pointer = 100;
word 1 dereferencedPointer = 1 deref pointer; // reads one word from absolute memory location stored in 'pointer', e.g. 100
```

#### ref
To grab variable absolute addresses that persist across function calls, use the ref operator.
Here, no 'casting' is done.
When invoked, the compiler first flushes the variable being referenced from registers.
Then, the compiler reads the variable's absolute memory offset into a register temporary that will persist.
```rust
// ref [variable]
// [variable] can be either a compiler temporary (e.g (5+3)) or a variable.
word 1 x = 10; // x now lives in registers since its of size 1
word 1 xptr = ref x; // x is flushed to stack and its stack offset + stack pointer is loaded into xptr
```

### Arithmetic
Arithmetic is optimized for 32-bit variables, as bigger vars must be written through to stack during each op.
A virtual register file where each register has a permanence allows compiler temporaries and variables to persist in registers.
Arithmetic operations are standard, with compiler accepting plus, minus, times, and division following c syntax.
Parentheses are supported and all operators evaluate with left-first precedence.