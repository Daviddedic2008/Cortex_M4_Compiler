# Language Documentation

**Lightweight language made for 32 bit arithmetic and data structures**

Single pass compiler, greedy register allocator(that spills based on loop depth and recent use), backpatching for jumps, constant folding, lazy register storage, lazy flag storage.
This compiler is machine-dependent and relies on a standard "word" size, which is 32 bits for the Cortex M4. This is done to force the programmer to optimize properly.
Memory footprint is small, with the binary being <128KB and the overall RAM usage(accounting for stack too) being sub-50KB.

---

## Syntax

## Variable Declaration

Variables are statically sized(not really typed). They can be sized to multiples of 32 bit words. Each declaration automatically allocates stack space for said variable. Variables that are one word can live in registers, while bigger variables are treated as write-through. Therefore, use one word variables for values that must stay in registers to be accessed easily.

```rust
// word [count] name = value;
// [count] must be a constant. It is optional. No [count] will default to size of one word
// [count] is optional, compiler defaults size to one word
word v; // declares 1 word variable
word 1 variable;	// simply declares it, no assignment
word var; // declares a 1 word variable
word 1 x = 10;	// a single 32-bit integer and assigns 10 to it
word 4 buffer = 0;	// reserves 16 bytes (4 words) on the stack and zeros the variable

```

---

## Variable Usage

Variables can be accessed as either flushable or not. Labeling a variable as flushable will make the compiler force stack writeback for the variable in that said occurrence. This keyword will only take effect on ONE occurrence at a time.

```rust
word 1 memoryLocation;
memoryLocation = 100; // you want this variable to write back to stack always, for some reason
flush memoryLocation = 5; // forces stack writeback in this case
memoryLocation = 5; // if memoryLocation is already in registers, load there


```

---

## Pointer Usage

Pointers are simply 1 word variables. This compiler is made for 32 bit systems, so therefore only accepts 32 bit pointers.

### DEREF

To get data stored at memory locations, use the DEREF operator. Pointer 'casting' is done at dereferencing, and therefore the operator takes a dereference size too. The size being dereferenced is static. you cannot dereference a dynamic amount of words.

Dereferences can be specified as volatile to ensure reading from stack. Volatile and non-constant(dereferencing a memory address not known at compile time) dereferences are a sync point where registers are flushed to stack. This allows for accurate control of GPIO regs, etc.

```rust
// [size] deref [address];
// [size] must be a constant. [address] can be a variable or constant
1 deref 100;	// reads one word from absolute memory location 100. COULD use values already in registers
1 volatile deref 100 // reads one word from absolute memory location 100. FORCES register sync and reads from stack.
word 1 pointer = 100;
word 1 dereferenced_pointer = 1 deref pointer; // reads one word from absolute memory location stored in 'pointer', e.g. 100

```

**IMPORTANT NOTE**
deref can also be used to write back to memory locations.

```rust
// [size] deref [address] = [value];
// [size] must be a constant. [address] can be a variable or constant. [value] can be a variable or constant.
1 deref 100 = 5; // write 5 to the word at memory location 100. Careful, may be unaligned!
word 1 x = 5; word 1 ref_x = ref x; // flush x to stack and grab reference
1 deref ref_x = 10; // load 10 into x
// NOTE: deref flushes all dirty registers to stack. USE POINTER OPERATIONS SPARINGLY.

```

### REF

To grab variable absolute addresses, use the REF operator. Here, no 'casting' is done. When invoked, the operator and its one operand are folded into a single constant memory location for local offsets or are added to a copy of the stack pointer register for absolute offsets

```rust
// ref [variable]
// [variable] can be either a compiler temporary (e.g (5+3)) or a variable.
word 1 x = 10; // x now lives in registers since its of size 1
word 1 x_ptr = ref x; // x's stack offset + stack pointer is loaded into xptr

```

---

## Arithmetic

Arithmetic is optimized for 32-bit variables, as bigger vars must be written through to stack during each op. A virtual register file where each register has a permanence allows compiler temporaries and variables to persist in registers. Arithmetic operations are standard, with compiler accepting plus, minus, times, and division following c syntax. Parentheses are supported and all operators evaluate with left-first precedence.

* **Important Differences:** * Only the first 32 bits of values will be multiplied/divided. Long multiplication/division isn't supported.
* Addition works with any sizes
* USE 32 BIT VALUES FOR ARITHMETIC! Using larger values will result in unneeded load/store pairs.

---

## Condition Evaluation

### Comparisons

Available comparisons: GREATER, EQUALS, LESS, NOT EQUALS, GREATER EQUALS, LESS EQUALS

Comparisons first "live" in flags, only being transferred to registers if another comparison is called.

```rust
// [o1] comparator [o2]
// [o1] and [o2] can be constant or variables or expressions
word 1 x = 0; // declare x
x = x + (x equals 0); // uses ITE(If-Then-Else) opcode to conditionally add 1 or nothing.
word 1 y = 5;
word z = y greater equals 7; // one comparison, doesnt actually do two.
y = (y equals 0) + (x equals 0); // only 1 can be stored in flags at a time. y == 0 result is loaded into registers.

```

---

## Branching

All branches are made to handle either direct comparisons, variable inputs, or constant inputs

### IF

```rust
// if [cond] {...} NOTE: parentheses enclosing condition are optional!
// [cond] can be a variable, comparison, expression, or constant
word x = 5;
if(x equals 5){ // emits a conditional branch
	x = 7;
}
if x{x = x + 5;} // also emits a conditional branch, comparing x to 0
if 1{x = 5;} // no comparison, either emits or doesnt emit code based on constant

```

IF branches default to the 32 bit conditional branching opcode, which is slighlty space-inefficient. This is a very minute detail but keep this in mind if you have 100's of ifs in a row.

### WHILE

```rust
// while [cond] {...} NOTE: parentheses enclosing condition are optional!
// [cond] can be a variable, comparison, expression, or constant
word x = 5;
while(x equals 5){ // emits a conditional branch
	x = 7;
}
while x{x = x + 5;} // also emits a conditional branch, comparing x to 0
while 1{x = 5;} // no comparison, makes an infinite loop

```

WHILE branches can be used to make infinite loops with no comparison penalty. They do not default to the 32-bit branching opcode, making them slightly more cache friendly than ifs :)

---

## Comments

Comments are not c-style, I haven't made multi-char delimiters yet. 

```rust
// although i'm writing comments in traditional c-style in the code segments here:
// the REAL language has no line comments, only block comments. The comment character is #.

# [comment] #
// comment can be anything

if x equals 5{
	# hey hey comment! x+5. This wont execute. #
	x = 5;
}

```

## Arrays

Arrays do not formally exist, but here every variable can be treated as a pseudo-array.

```rust
word 100 arr; // define array of 100 32-bit words

1 deref (ref arr + 4) = 5; // write 5 to the 4th word in the array. This compiles to one move immediate and one store immediate.


```