#include <stdio.h>
#include <stdint.h>
#include "compiler.h"

#if defined(__GNUC__) || defined(__clang__)
	#define forceinline static inline __attribute__((always_inline))
#else
	#define forceinline static inline
#endif

#if defined(__x86_64__) || defined(_M_X64) || defined(__aarch64__) || defined(__LP64__)
    #define unionType int64_t
    #define BITS 64
#else
#define unionType int32_t
    #define BITS 32
#endif

#define UINT32_MAX 0xFFFFFFFF
#define UINT16_MAX 0xFFFF
#define UINT8_MAX 0xFF

//#############################################################################################################################################
// TOKENIZER

enum tokenType {
	opToken, identifierToken, keywordToken, constantToken,
	sizeToken, clampToken, endLine, nullToken
};

enum opSubtype {
	opAdd, opSub, opIncrement, opDecrement, opMul, opScaleM, opDiv, opScaleD, opNegate,
	opRightShift, opLeftShift,
	opEqual, opFree, opReference, opWriteback, opWritebackArr, opDereferenceArr,
	opDereference, opBitwiseOr, opBitwiseAnd, opBitwiseNot, opBitwiseXor, opLogicalAnd,
	opLogicalOr, opLogicalNot, opCmpGreater, opCmpLess, opCmpEqual, opCmpGEqual, opCmpNEqual, opCmpLEqual,
	opDereferenceVolatile, opWritebackVolatile
};

enum clampSubtype {
	parenthesesL, parenthesesR,
	curlyBL, curlyBR
};

enum keywordSubtype {
	ifKey, whileKey, elseKey,
	breakKey, continueKey, flushKey, volatileKey, argumentKey, returnKey
};

enum sizeSubtype {
	wordSize, byteSize, halfSize
};
#define byteSz UINT32_MAX
#define halfSz (UINT32_MAX-1)

typedef struct{
	char* str; uint16_t len;
	uint8_t type, subtype;
}token;

#define isNullChar(c) (c == ' ' || c == '\n' || c == '\t')

const char ops[] = { '+', '-', '*', '%', '=', '/', '\\', '>', '<', '&', '|', '^', '!', 'T'};
const char singleTokens[] = { '(', ')', '{', '}', ';', 'T'};
uint8_t isOp(const char c) {
	for (char* tmp = ops; *tmp != 'T'; tmp++) { if (*tmp == c) { return 1; } }
	return 0;
}

uint8_t isSingle(const char c) {
	for (char* tmp = singleTokens; *tmp != 'T'; tmp++) { if (*tmp == c) { return 1; } }
	for (char* tmp = ops; *tmp != 'T'; tmp++) { if (*tmp == c) { return 1; } }
	return 0;
}

uint8_t tokenCmpLiteral(token s, const char* lit){
	uint8_t tIdx = 0; for(; *lit != '\0'; tIdx++, lit++){
		if(tIdx == s.len) return 0;
		if(s.str[tIdx] != *lit) return 0;
	} return tIdx == s.len;
}

char* src; token curToken;
forceinline void setSource(const char* c){src = c;}

void nextToken(){
	while(isNullChar(*src))src++;
	if(*src == '#'){do{src++;}while(*src != '#'); src++; nextToken(); return;}
	curToken = (token){(void*)0, 0, nullToken, 0}; if(*src == '\0')return;
	curToken.str = src; curToken.len = 0;
	if(isSingle(*src)){src++; curToken.len++;}
	else{while(!isNullChar(*src) && !isSingle(*src)){src++; curToken.len++;}}
	switch(*(curToken.str)){
		case '+': curToken.type = opToken; curToken.subtype = opAdd;return;
		case '-': curToken.type = opToken; curToken.subtype = opSub;return;
		case '*': curToken.type = opToken; curToken.subtype = opMul;return;
		case '/': curToken.type = opToken; curToken.subtype = opDiv;return;
		case '=': curToken.type = opToken; curToken.subtype = opEqual;return;
		case '>': curToken.type = opToken; curToken.subtype = opRightShift;return;
		case '<': curToken.type = opToken; curToken.subtype = opLeftShift;return;
		case '|': curToken.type = opToken; curToken.subtype = opBitwiseOr;return;
		case '^': curToken.type = opToken; curToken.subtype = opBitwiseXor;return;
		case '&': curToken.type = opToken; curToken.subtype = opBitwiseAnd;return;
		case '!': curToken.type = opToken; curToken.subtype = opBitwiseNot;return;
		case '(': curToken.type = clampToken; curToken.subtype = parenthesesL;return;
		case ')': curToken.type = clampToken; curToken.subtype = parenthesesR;return;
		case '{': curToken.type = clampToken; curToken.subtype = curlyBL;return;
		case '}': curToken.type = clampToken; curToken.subtype = curlyBR;return;
		case ';': curToken.type = endLine; curToken.subtype = 0; return;
		case 'r': if(tokenCmpLiteral(curToken, "ref")){curToken.type = opToken; curToken.subtype = opReference; return;}
		else if(tokenCmpLiteral(curToken, "return")){curToken.type = keywordToken; curToken.subtype = returnKey; return;} break;
		case 'd': if(tokenCmpLiteral(curToken, "deref")){curToken.type = opToken; curToken.subtype = opDereference; return;}
		else if(tokenCmpLiteral(curToken, "decrement")){curToken.type = opToken; curToken.subtype = opDecrement; return;} break;
		case 'o': if(tokenCmpLiteral(curToken, "or")){curToken.type = opToken; curToken.subtype = opLogicalOr; return;} break;
		case 'a': if(tokenCmpLiteral(curToken, "and")){curToken.type = opToken; curToken.subtype = opLogicalAnd; return;}
		else if(tokenCmpLiteral(curToken, "argument")){curToken.type = keywordToken; curToken.subtype = argumentKey; return;} break;
		case 'n': if(tokenCmpLiteral(curToken, "not")){curToken.type = opToken; curToken.subtype = opLogicalNot; return;} break;
		case 'e': if(tokenCmpLiteral(curToken, "equals")){curToken.type = opToken; curToken.subtype = opCmpEqual; return;}
		else if(tokenCmpLiteral(curToken, "else")){curToken.type = keywordToken; curToken.subtype = elseKey; return;} break;
		case 'g': if(tokenCmpLiteral(curToken, "greater")){curToken.type = opToken; curToken.subtype = opCmpGreater; return;} break;
		case 'l': if(tokenCmpLiteral(curToken, "less")){curToken.type = opToken; curToken.subtype = opCmpLess; return;} break;
		case 'i': if(tokenCmpLiteral(curToken, "if")){curToken.type = keywordToken; curToken.subtype = ifKey; return;}
		else if(tokenCmpLiteral(curToken, "in")){curToken.type = opToken; curToken.subtype = opDereferenceArr; return;} break;
		case 'c': if(tokenCmpLiteral(curToken, "continue")){curToken.type = keywordToken; curToken.subtype = continueKey; return;} break;
		case 'b': if(tokenCmpLiteral(curToken, "break")){curToken.type = keywordToken; curToken.subtype = breakKey; return;}
		else if(tokenCmpLiteral(curToken, "byte")){curToken.type = sizeToken; curToken.subtype = byteSize; return;} break;
		case 'h': if(tokenCmpLiteral(curToken, "half")){curToken.type = sizeToken; curToken.subtype = halfSize; return;} break;
		case 'f': if(tokenCmpLiteral(curToken, "flush")){curToken.type = keywordToken; curToken.subtype = flushKey; return;}
		else if(tokenCmpLiteral(curToken, "free")){curToken.type = opToken; curToken.subtype = opFree; return;} break;
		case 'v': if(tokenCmpLiteral(curToken, "volatile")){curToken.type = keywordToken; curToken.subtype = volatileKey; return;} break;
		case 'w': if(tokenCmpLiteral(curToken, "while")){curToken.type = keywordToken; curToken.subtype = whileKey; return;}
		else if(tokenCmpLiteral(curToken, "word")){curToken.type = sizeToken; curToken.subtype = 0; return;} break;
	} if(*(curToken.str) >= '0' && *(curToken.str) <= '9'){curToken.type = constantToken; curToken.subtype = wordSize; return;}
	curToken.type = identifierToken;
}

//#############################################################################################################################################
// HEX EMITTER

typedef enum specialRegs {
	scratchReg1 = 12,
	stackPointerReg = 13
}specialRegs;

typedef enum opcode {
	movw_reg_reg_32, movw_lit_32, movt_lit_32, 
	ldrw_imm_32, strw_imm_32, ldrw_reg_32, strw_reg_32,
	ldrbw_imm_32, strbw_imm_32, ldrbw_reg_32, strbw_reg_32,
	ldrhw_imm_32, strhw_imm_32, ldrhw_reg_32, strhw_reg_32,
	subw_imm_32, addw_imm_32, subw_reg_32, addw_reg_32,
	subs_imm_32, subs_reg_32, adds_imm_32, adds_reg_32, lsr_imm_32, lsl_imm_32, lsr_reg_32, lsl_reg_32,
	mulw_reg_32, divw_reg_32, ite_32, it_32,
	cmp_reg_32, cmp_imm_32, pop_32, push_32,
	eors_reg_32, eors_imm_32, orrs_reg_32, andw_imm_32, andw_reg_32, orrs_imm_32,
	mvn_imm_32, mvn_reg_32, b_imm_32, b_reg_32, bc_reg_32, bc_imm_32,
	ldrb_imm_16, strb_imm_16, ldrh_imm_16, strh_imm_16,
	b_imm_16, b_reg_16, bc_imm_16, bc_reg_16, mov_lit_16, ldr_imm_16, ldr_gpr_imm_16, str_imm_16, mov_reg_reg_16, add_reg_16
}opcode;
enum ldOrStr{noLdStr, isLoad, isStore};
typedef struct{
	uint8_t destReg : 2;
	uint8_t readRegs : 3;
	uint8_t isLoadOrStore : 2;
	uint8_t locked : 1;
	uint8_t flagUsage;
}opcodeTag;
enum flagMods{fNone, fSet, fRead};
enum lockedMods{unlkInstr, lkInstr};
const opcodeTag opcodeTags[60] = {
    [movw_reg_reg_32] = {1, 2, noLdStr, unlkInstr, fNone}, 
    [movw_lit_32]     = {1, 0, noLdStr, unlkInstr, fNone}, 
    [movt_lit_32]     = {1, 0, noLdStr, unlkInstr, fNone},

    [ldrw_imm_32]  = {2, 1, isLoad, unlkInstr, fNone}, [ldrw_reg_32]  = {2, 5, isLoad, unlkInstr, fNone}, 
    [ldrbw_imm_32] = {2, 1, isLoad, unlkInstr, fNone}, [ldrbw_reg_32] = {2, 5, isLoad, unlkInstr, fNone}, 
    [ldrhw_imm_32] = {2, 1, isLoad, unlkInstr, fNone}, [ldrhw_reg_32] = {2, 5, isLoad, unlkInstr, fNone},
    [ldr_imm_16]   = {1, 1, isLoad, unlkInstr, fNone}, [ldr_gpr_imm_16] = {1, 1, isLoad, unlkInstr, fNone}, 
    [ldrb_imm_16]  = {1, 1, isLoad, unlkInstr, fNone}, [ldrh_imm_16] = {1, 1, isLoad, unlkInstr, fNone},
	
	[pop_32] = {1, 0, isLoad, fNone}, [push_32] = {0, 1, noLdStr, fNone},

    [strw_imm_32]  = {0, 3, isStore, unlkInstr, fNone}, [strw_reg_32]  = {0, 7, isStore, unlkInstr, fNone},
    [strbw_imm_32] = {0, 3, isStore, unlkInstr, fNone}, [strbw_reg_32] = {0, 7, isStore, unlkInstr, fNone},
    [strhw_imm_32] = {0, 3, isStore, unlkInstr, fNone}, [strhw_reg_32] = {0, 7, isStore, unlkInstr, fNone},
    [str_imm_16]   = {0, 3, isStore, unlkInstr, fNone}, [strb_imm_16]  = {0, 3, isStore, unlkInstr, fNone}, 
    [strh_imm_16]  = {0, 3, isStore, unlkInstr, fNone},

    [subw_imm_32] = {1, 2, noLdStr, unlkInstr, fNone}, [addw_imm_32] = {1, 2, noLdStr, unlkInstr, fNone}, 
    [subw_reg_32] = {1, 6, noLdStr, unlkInstr, fNone}, [addw_reg_32] = {1, 6, noLdStr, unlkInstr, fNone},
    [subs_imm_32] = {1, 2, noLdStr, unlkInstr, fSet},  [subs_reg_32] = {1, 6, noLdStr, unlkInstr, fSet}, 
    [mulw_reg_32] = {1, 6, noLdStr, unlkInstr, fNone}, [divw_reg_32] = {1, 6, noLdStr, unlkInstr, fNone},
    [lsr_imm_32]  = {1, 2, noLdStr, unlkInstr, fNone}, [lsl_imm_32]  = {1, 2, noLdStr, unlkInstr, fNone}, 
    [lsr_reg_32]  = {1, 6, noLdStr, unlkInstr, fNone}, [lsl_reg_32]  = {1, 6, noLdStr, unlkInstr, fNone},
    [eors_reg_32] = {1, 6, noLdStr, unlkInstr, fSet},  [eors_imm_32] = {1, 2, noLdStr, unlkInstr, fSet}, 
    [orrs_reg_32] = {1, 6, noLdStr, unlkInstr, fSet},  [andw_imm_32] = {1, 2, noLdStr, unlkInstr, fNone}, 
    [andw_reg_32] = {1, 6, noLdStr, unlkInstr, fNone}, [orrs_imm_32] = {1, 2, noLdStr, unlkInstr, fSet}, 
    [mvn_imm_32]  = {1, 0, noLdStr, unlkInstr, fNone}, [mvn_reg_32]  = {1, 2, noLdStr, unlkInstr, fNone},
    [mov_lit_16]  = {1, 0, noLdStr, unlkInstr, fNone}, [mov_reg_reg_16] = {1, 2, noLdStr, unlkInstr, fNone}, 
    [adds_reg_32]  = {1, 6, noLdStr, unlkInstr, fSet}, [adds_imm_32]  = {1, 2, noLdStr, unlkInstr, fSet},

    [cmp_reg_32] = {0, 3, noLdStr, unlkInstr, fSet}, [cmp_imm_32] = {0, 1, noLdStr, unlkInstr, fSet}, 
    [ite_32] = {0, 0, noLdStr, lkInstr, fRead},    [it_32] = {0, 0, noLdStr, lkInstr, fRead}, 
    [b_imm_32] = {0, 0, noLdStr, lkInstr, fNone},  [b_reg_32] = {0, 1, noLdStr, lkInstr, fNone}, 
    [bc_reg_32] = {0, 1, noLdStr, lkInstr, fRead}, [bc_imm_32] = {0, 0, noLdStr, lkInstr, fRead},
    [b_imm_16] = {0, 0, noLdStr, lkInstr, fNone},  [b_reg_16] = {0, 1, noLdStr, lkInstr, fNone}, 
    [bc_imm_16] = {0, 0, noLdStr, lkInstr, fRead}, [bc_reg_16] = {0, 1, noLdStr, lkInstr, fRead}
};

#define lim32 bc_imm_32

enum armFlags{
	flag_eq, flag_ne, flag_lt, flag_gt, flag_ge, flag_le
};
const uint8_t armFlagCodes[6] = {
	[flag_eq] = 0x0, [flag_ne] = 0x1, [flag_lt] = 0xb, [flag_gt] = 0xc, [flag_ge] = 0xa, [flag_le] = 0xd
};
enum argType{registerArg, constArg};
typedef struct{uint16_t val; uint8_t type}arg;
typedef struct{
	uint8_t code; uint8_t numArgs, chainId;
	arg args[3];
}instruction;

uint16_t cId = 0, sc = 0;
#define createChain() cId = ++sc
#define endChain() cId = 0

#define makeInstruction(code) (instruction){.code = code, .numArgs = 0, .chainId = cId};
#define frameDepth 16
uint32_t progAddr, progAddrC; uint8_t* outputBuf; uint8_t modifierFrame;
instruction instructionFrame[frameDepth]; uint8_t numInstructions = 0;
instruction prevInstruction = (instruction){.code = 0xFF, .numArgs = 0xFF, .chainId = 0};
uint8_t instructionBlockers[frameDepth]; uint8_t i0f = 0; uint16_t takenBlocks = 0;
forceinline uint8_t addBlock(){
	for(uint16_t mask = 1, i = 0; i < frameDepth; i++, mask << 1){
		if(!(takenBlocks | mask)){takenBlocks |= mask; instructionBlockers[i] = numInstructions; return i;}
	}
}
forceinline void endBlock(const uint8_t bId){takenBlocks &= ~(1 << bId);}

#define maxGPRegs 12
enum isKnown{unknownVal, knownVal};
typedef struct{uint32_t value; uint8_t status}regValue;
regValue constInRegs[maxGPRegs];
forceinline void clearConstRegs(){
	for(uint8_t r = 0; r < maxGPRegs; r++) constInRegs[r] = (regValue){0, unknownVal};
}

#define args (instructionFrame[instructionIdx - 1].args)
forceinline void emitHex(const uint8_t instructionIdx) {
    const uint8_t code = instructionFrame[instructionIdx - 1].code;
    uint32_t emit = 0; uint8_t wrSz = 4;
	int32_t cImm = -1;
    switch(code) {
    case mov_lit_16: emit |= (1 << 13) | (args[0].val << 8) | args[1].val; wrSz = 2; cImm = args[1].val; break;
    case movw_lit_32: case movt_lit_32: emit |= (0b1111001001 << 22) | ((code == movt_lit_32) << 23) | (((args[1].val >> 11) & 1) << 26) | ((args[1].val >> 12 & 0xF) << 16) | ((args[1].val >> 8 & 0x7) << 12) | (args[0].val << 8) | (args[1].val & 0xFF); 
	cImm = args[1].val << (code == movt_lit_32 ? 16 : 0); break;
    case mov_reg_reg_16: emit |= 0x4600 | ((args[0].val & 8) << 4) | (args[1].val << 3) | (args[0].val & 7); wrSz = 2; break;
    case movw_reg_reg_32: emit |= (0b1110101001001111 << 16) | (args[0].val << 8) | args[1].val; break;
    case mvn_reg_32: emit |= (0b1110101001101111 << 16) | (args[0].val << 8) | args[1].val; break;
    case ldr_imm_16: emit |= (0b10011 << 11) | (args[0].val << 8) | (args[1].val >> 2); wrSz = 2; break;
    case ldrw_imm_32: emit |= (0b111110001101 << 20) | (args[0].val << 16) | (args[1].val << 12) | args[2].val; break;
    case ldr_gpr_imm_16: emit |= (0b01101 << 11) | (args[2].val << 6) | (args[0].val << 3) | args[1].val; wrSz = 2; break;
    case ldrw_reg_32: emit |= (0b111110000101 << 20) | (args[0].val << 16) | (args[1].val << 12) | args[2].val; break;
    case ldrb_imm_16: emit |= (0b01111 << 11) | (args[2].val << 6) | (args[0].val << 3) | args[1].val; wrSz = 2; break;
    case ldrbw_imm_32: emit |= (0b111110001001 << 20) | (args[0].val << 16) | (args[1].val << 12) | args[2].val; break;
    case ldrh_imm_16: emit |= (0b10001 << 11) | (args[2].val << 6) | (args[0].val << 3) | args[1].val; wrSz = 2; break;
    case ldrhw_imm_32: emit |= (0b111110001011 << 20) | (args[0].val << 16) | (args[1].val << 12) | args[2].val; break;
    case ldrhw_reg_32: emit |= (0b111110000011 << 20) | (args[0].val << 16) | (args[1].val << 12) | args[2].val; break;
    case str_imm_16: emit |= (0b10010 << 11) | (args[1].val << 8) | args[2].val; wrSz = 2; break;
    case strw_imm_32: emit |= (0b111110001100 << 20) | (args[0].val << 16) | (args[1].val << 12) | args[2].val; break;
    case strw_reg_32: emit |= (0b111110000100 << 20) | (args[0].val << 16) | (args[1].val << 12) | args[2].val; break;
    case strb_imm_16: emit |= (0b01110 << 11) | (args[2].val << 6) | (args[0].val << 3) | args[1].val; wrSz = 2; break;
    case strbw_imm_32: emit |= (0b111110001000 << 20) | (args[0].val << 16) | (args[1].val << 12) | args[2].val; break;
    case strh_imm_16: emit |= (0b10000 << 11) | (args[2].val << 6) | (args[0].val << 3) | args[1].val; wrSz = 2; break;
    case strhw_imm_32: emit |= (0b111110001010 << 20) | (args[0].val << 16) | (args[1].val << 12) | args[2].val; break;
    case strhw_reg_32: emit |= (0b111110000010 << 20) | (args[0].val << 16) | (args[1].val << 12) | args[2].val; break;
    case addw_imm_32: case adds_imm_32: emit |= (0b11110001000 << 21) | ((code == adds_imm_32) << 20) | (args[1].val << 16) | (args[0].val << 8) | (args[2].val & 0xFF); break;
    case subw_imm_32: case subs_imm_32: emit |= (0b11110001101 << 21) | ((code == subs_imm_32) << 20) | (args[1].val << 16) | (args[0].val << 8) | (args[2].val & 0xFF); break;
    case addw_reg_32: case adds_reg_32: emit |= (0b11101011000 << 21) | ((code == adds_reg_32) << 20) | (args[1].val << 16) | (args[0].val << 8) | args[2].val; break;
    case subw_reg_32: case subs_reg_32: emit |= (0b11101011101 << 21) | ((code == subs_reg_32) << 20) | (args[1].val << 16) | (args[0].val << 8) | args[2].val; break;
    case lsl_imm_32: case lsr_imm_32: emit |= (0b1110101001001111 << 16) | (args[0].val << 8) | (args[1].val) | ((args[2].val & 0x1C) << 10) | ((args[2].val & 0x3) << 6) | (code == lsr_imm_32 ? (0b01 << 4) : 0); break;
    case lsl_reg_32: case lsr_reg_32: emit |= (0b111110100000 << 20) | (code == lsr_reg_32 ? (1 << 21) : 0) | (args[1].val << 16) | (0b1111 << 12) | (args[0].val << 8) | args[2].val; break;
    case mulw_reg_32: case divw_reg_32: emit |= (0b111110110000 << 20) | (code == divw_reg_32 ? (1 << 20) : 0) | (args[1].val << 16) | (0b1111 << 12) | (args[0].val << 8) | args[2].val; break;
	case andw_imm_32: emit |= (0xF000 << 16) | (args[1].val << 16) | ((args[2].val & 0x80) << 19) | (args[0].val << 8) | ((args[2].val & 0x70) << 8) | (args[2].val & 0x7F); wrSz = 4; break;
	case andw_reg_32: emit |= (0b111010100000 << 20) | (args[1].val << 16) | (args[0].val << 8) | args[2].val; break;
	case orrs_imm_32: emit |= (0b111100000101 << 20) | (args[1].val << 16) | (args[0].val << 8) | (args[2].val & 0xFF); break;
	case orrs_reg_32: emit |= (0b111010100101 << 20) | (args[1].val << 16) | (args[0].val << 8) | args[2].val; break;
	case eors_imm_32: emit |= (0b111100001001 << 20) | (args[1].val << 16) | (args[0].val << 8) | (args[2].val & 0xFF); break;
	case eors_reg_32: emit |= (0b111010101001 << 20) | (args[1].val << 16) | (args[0].val << 8) | args[2].val; break;
	case cmp_reg_32: emit |= (0b111010111011 << 20) | (args[0].val << 16) | (0b1111 << 12) | args[1].val; break;
	case cmp_imm_32: emit |= (0b111100011011 << 20) | (args[0].val << 16) | (0b1111 << 12) | (args[1].val & 0xFF); break;
	case it_32: emit |= 0xBF00 | (args[0].val << 4) | 0b1000; wrSz = 2; break;
	case ite_32: emit |= 0xBF00 | (args[0].val << 4) | 0b0100; wrSz = 2; break;
	case bc_imm_32:{const uint8_t sign = ((int16_t)args[1].val < 0 ? 1 : 0);
	emit |= (0b1111 << 28) | (sign << 26)| (args[0].val << 22) | ((args[1].val & 0b1111110000000000) << 6) | (sign << 13) | (1 << 15) | (sign << 11) | (args[1].val & 0b1111111111); break;}
	case b_imm_32:{const uint8_t sign = ((int16_t)args[0].val < 0 ? 1 : 0);
    emit |= (0b1111 << 28) | (sign << 26) | ((sign ? 0x3FF : 0) << 16) | ((args[0].val & 0xF800) << 5) | (0x17 << 11) | (args[0].val & 0x7FF);
    break;}
    default: wrSz = 2; emit = 0xBF00; break;
	} if(opcodeTags[code].destReg){constInRegs[args[opcodeTags[code].destReg-1].val].status = unknownVal;}
	if(cImm != -1){
		constInRegs[args[opcodeTags[code].destReg-1].val].status = knownVal;
		switch(code){
			case movt_lit_32:
			constInRegs[args[opcodeTags[code].destReg-1].val].value |= cImm; break;
			default: constInRegs[args[opcodeTags[code].destReg-1].val].value = cImm;
		}
	}
	if(wrSz == 4) {
		outputBuf[progAddr++] |= (emit >> 16) & 0xFF; 
		outputBuf[progAddr++] |= (emit >> 24) & 0xFF;
		outputBuf[progAddr++] |= emit & 0xFF; 
		outputBuf[progAddr++] |= (emit >> 8) & 0xFF;
	} else {
		outputBuf[progAddr++] |= emit & 0xFF; 
		outputBuf[progAddr++] |= (emit >> 8) & 0xFF;
	}
}
#undef args

forceinline void shiftInstructionFrame(const uint8_t rmIdx){
	for(uint8_t i1 = rmIdx, i2 = rmIdx + 1; i2 < frameDepth; i2++, i1++) instructionFrame[i1] = instructionFrame[i2];
}

forceinline int8_t getWriteReg(const instruction i){ 
	return (opcodeTags[i.code].destReg > 0) ? (i.args[opcodeTags[i.code].destReg - 1].val) : -1;
}
forceinline uint8_t writesToRegister(const instruction i, const uint8_t reg){
	if(opcodeTags[i.code].destReg - 1 && i.args[opcodeTags[i.code].destReg - 1].val == reg) return 1; return 0; 
}
forceinline uint8_t readsFromRegister(const instruction i, const int8_t reg){
	if(reg == -1) return 0;
	for(uint8_t o = 0; o < 3; o++){
		if((opcodeTags[i.code].readRegs >> o) & 1 && i.args[o].val == reg) return 1;
	} return 0;
}
forceinline uint8_t isPipelineBubble(const instruction i1, const instruction i2){
	if(opcodeTags[i1.code].isLoadOrStore != isLoad)
	if(i1.code == 0xFF) return 0; if(opcodeTags[i1.code].isLoadOrStore != noLdStr && opcodeTags[i1.code].isLoadOrStore == opcodeTags[i2.code].isLoadOrStore){
		return 1;
	} if(readsFromRegister(i2, getWriteReg(i1))) return 1;
	return 0;
}
enum orderTypes{noOrder, requiredOrder, redundantOrder};
forceinline uint8_t orderRequirements(const instruction i1, const instruction i2){
	const int8_t rw1 = getWriteReg(i1); const int8_t rw2 = getWriteReg(i2);
	if(i1.chainId == i2.chainId && i1.chainId != 0) return requiredOrder;
	if(readsFromRegister(i2, rw1) || readsFromRegister(i1, rw2) || opcodeTags[i1.code].locked || opcodeTags[i2.code].locked || (rw1 == rw2 && rw1 != -1)) return requiredOrder;
	if(opcodeTags[i1.code].flagUsage == fRead && opcodeTags[i2.code].flagUsage == fSet) return requiredOrder;
	if(opcodeTags[i1.code].flagUsage == fSet && (opcodeTags[i2.code].flagUsage == fRead || opcodeTags[i2.code].flagUsage == fSet)) return requiredOrder;
	if(rw1 == rw2 && rw1 != -1) return redundantOrder; 
	return noOrder;
}

forceinline uint8_t checkBlocking(const uint8_t i1, const uint8_t i2){
	for(uint16_t mask = 1, i = 0; i < frameDepth; i++, mask << 1){
		if(mask & takenBlocks && instructionBlockers[i] > i1 && instructionBlockers[i] <= i2)
		return 1;
	} return 0;
}

forceinline void popInstruction(){
	uint8_t popInst = 0; uint8_t loadGroup = opcodeTags[prevInstruction.code].isLoadOrStore == isLoad;
	if(isPipelineBubble(prevInstruction, instructionFrame[0])){
		for(uint8_t checkPop = 1; checkPop < numInstructions; checkPop++){
			if(checkBlocking(0, checkPop)) continue;
			for(uint8_t i2 = 0; i2 < checkPop; i2++) if(orderRequirements(instructionFrame[i2], instructionFrame[checkPop]) != noOrder) goto skpLp;
			popInst = loadGroup > 1 ? popInst : checkPop; loadGroup += opcodeTags[instructionFrame[checkPop].code].isLoadOrStore == isLoad; skpLp:;
		}
	}
	for(uint8_t bIdx = 0; bIdx < frameDepth; bIdx++){
		if(((1<< bIdx) & takenBlocks) && instructionBlockers[bIdx] >= numInstructions){
			if(instructionBlockers[bIdx] > 0) instructionBlockers[bIdx]--;
			else endBlock(bIdx);
		}
	}
	numInstructions--; emitHex(popInst+1); prevInstruction = instructionFrame[popInst]; shiftInstructionFrame(popInst);
}

void emitOpcode(const uint8_t code) {
	switch (code) {
	case movw_reg_reg_32: printf("MOVW_REG_REG_32"); break;
	case movw_lit_32:    printf("MOVW_LIT_32"); break;
	case movt_lit_32:    printf("MOVT_LIT_32"); break;
	case mvn_reg_32:    printf("MVN_REG_32"); break;
	case ldrw_imm_32:    printf("LDRW_IMM_32"); break;
	case strw_imm_32:    printf("STRW_IMM_32"); break;
	case ldrw_reg_32:    printf("LDRW_REG_32"); break;
	case strw_reg_32:    printf("STRW_REG_32"); break;
	case ldrbw_imm_32:    printf("LDRBW_IMM_32"); break;
	case strbw_imm_32:    printf("STRBW_IMM_32"); break;
	case ldrbw_reg_32:    printf("LDRBW_REG_32"); break;
	case strbw_reg_32:    printf("STRBW_REG_32"); break;
	case ldrhw_imm_32:    printf("LDRHW_IMM_32"); break;
	case strhw_imm_32:    printf("STRHW_IMM_32"); break;
	case ldrhw_reg_32:    printf("LDRHW_REG_32"); break;
	case strhw_reg_32:    printf("STRHW_REG_32"); break;
	case mov_lit_16:     printf("MOV_LIT_16"); break;
	case str_imm_16:     printf("STR_IMM_16"); break;
	case ldr_imm_16:     printf("LDR_IMM_16"); break;
	case strb_imm_16:     printf("STRB_IMM_16"); break;
	case ldrb_imm_16:     printf("LDRB_IMM_16"); break;
	case strh_imm_16:     printf("STRH_IMM_16"); break;
	case ldrh_imm_16:     printf("LDRH_IMM_16"); break;
	case ldr_gpr_imm_16: printf("LDR_GPR_IMM_16"); break;
	case subw_imm_32:    printf("SUBW_IMM_32"); break;
	case addw_imm_32:    printf("ADDW_IMM_32"); break;
	case subw_reg_32:    printf("SUBW_REG_32"); break;
	case addw_reg_32:    printf("ADDW_REG_32"); break;
	case adds_reg_32:     printf("ADDS_REG_32"); break;
	case adds_imm_32:     printf("ADDS_IMM_32"); break;
	case mulw_reg_32:    printf("MULW_REG_32"); break;
	case divw_reg_32:    printf("DIVW_REG_32"); break;
	case eors_imm_32:	 printf("EORS_IMM_32"); break;
	case eors_reg_32:	 printf("EORS_REG_32"); break;
	case orrs_reg_32:	 printf("ORRS_REG_32"); break;
	case orrs_imm_32:	 printf("ORRS_IMM_32"); break;
	case pop_32: 		 printf("POP_32"); break;
	case push_32: 		 printf("PUSH_32"); break;
	case subs_reg_32:    printf("SUBS_REG_32"); break;
	case subs_imm_32:    printf("SUBS_IMM_32"); break;
	case andw_imm_32: 	 printf("ANDW_IMM_32"); break;
	case andw_reg_32: 	 printf("ANDW_REG_32"); break;
	case lsr_imm_32:	 printf("LSR_IMM_32"); break;
	case lsl_imm_32:	 printf("LSL_IMM_32"); break;
	case lsl_reg_32:	 printf("LSL_REG_32"); break;
	case lsr_reg_32:	 printf("LSR_REG_32"); break;
	case cmp_imm_32: 	 printf("CMP_IMM_32"); break;
	case cmp_reg_32: 	 printf("CMP_REG_32"); break;
	case ite_32:		 printf("ITE_32"); break;
	case it_32:		 	 printf("IT_32"); break;
	case b_imm_32:		 printf("B_IMM_32"); break;
	case bc_imm_32:		 printf("BC_IMM_32"); break;
	case bc_imm_16:		 printf("BC_IMM_16"); break;
	case mov_reg_reg_16: printf("MOV_REG_REG_16"); break;
	case b_imm_16:		 printf("B_IMM_16"); break;
	default:             printf("UNKNOWN_OP"); break;
	}
	if(numInstructions == frameDepth){
		popInstruction();
	} progAddrC += (code > lim32) ? 2 : 4;
	instructionFrame[numInstructions++] = makeInstruction(code);
	printf("\n");
	fflush(stdout);
}
forceinline void flushInstructionBuffer(){
	while(numInstructions > 0){
		popInstruction();
	}
}

forceinline void emitModifier(const uint8_t mod){
	printf(mod ? "NO CARRY\n" : "CARRY\n");
	modifierFrame = mod;
}

#define curInst instructionFrame[numInstructions-1]
forceinline void hexArg(const arg argV){
	curInst.args[curInst.numArgs++] = argV;
}
#undef curInst
void emitArgument(const uint32_t argV, const uint8_t sz) {
	hexArg((arg){argV, sz <= 4 ? registerArg : constArg});
	printf("ARG %d\n", argV);
}

forceinline void emitFlag(const uint8_t flag) {
	switch(flag){
		case flag_gt: printf("FLAG GT\n"); break;
		case flag_ge: printf("FLAG GE\n"); break;
		case flag_eq: printf("FLAG EQ\n"); break;
		case flag_ne: printf("FLAG NE\n"); break;
		case flag_le: printf("FLAG LE\n"); break;
		case flag_lt: printf("FLAG LT\n");
	} hexArg((arg){armFlagCodes[flag], constArg});
}

//#############################################################################################################################################
// INSTRUCTION GENERATION

enum registerStorageStatus{
	clean = 1, dirty = 2, locked = 3,
	empty = 4
};

typedef struct {
	uint32_t stackOffset;
	uint16_t dirty, priority;
}registerData;
typedef struct{
	uint32_t stackOffset;
	uint8_t dirty; uint8_t flagType;
}flagData;

#define maxFlags 4
registerData virtualRegFile[maxGPRegs]; // 10 data registers
flagData virtualFlags; // only one value can be in the flag reg at once

forceinline void initializeVirtualFlags(){
	virtualFlags = (flagData){UINT32_MAX, empty};
}
forceinline void initializeVirtualRegs(){
	for(uint8_t idx = 0; idx < maxGPRegs; idx++) virtualRegFile[idx] = (registerData){UINT32_MAX, empty, UINT16_MAX/2};
}

enum instructionTypes{storeImm16, loadImm16, storeImm32, loadImm32, storeReg32, loadReg32};
const uint8_t instructionMap[6][3] = {
    {str_imm_16, strb_imm_16, strh_imm_16}, // storeImm16
    {ldr_imm_16, ldrb_imm_16, ldrh_imm_16}, // loadImm16
    {strw_imm_32, strbw_imm_32, strhw_imm_32}, // storeImm32
    {ldrw_imm_32, ldrbw_imm_32, ldrhw_imm_32}, // loadImm32
    {strw_reg_32, strbw_reg_32, strhw_reg_32}, // storeReg32
    {ldrw_reg_32, ldrbw_reg_32, ldrhw_reg_32}  // loadReg32
};

void storeConstantInReg(const uint8_t reg, const uint32_t c) {
	if(reg < 8 && c < UINT8_MAX){emitOpcode(mov_lit_16); emitArgument(reg, 3); emitArgument(c, 8);}
	else{
		emitOpcode(movw_lit_32); emitArgument(reg, 4); emitArgument(c & 0xFFFF, 16);
		if(c > 65535){emitOpcode(movt_lit_32); emitArgument(reg, 4); emitArgument(c >> 16, 16);}
	}
}
void storeRegisterIntoStack(const uint8_t reg, const uint32_t stackOffset, const uint8_t sizeType) {
	if (stackOffset >= 4095) {
		storeConstantInReg(scratchReg1, stackOffset);
		emitOpcode(instructionMap[storeReg32][sizeType]); emitArgument(stackPointerReg, 4); emitArgument(reg, 4);
		emitArgument(scratchReg1, 4);
	}
	else if(stackOffset < 1021 && stackOffset % 4 == 0 && reg < 8){emitOpcode(instructionMap[storeImm16][sizeType]); emitArgument(reg, 3); emitArgument(stackOffset, 8);}
	else{
		emitOpcode(instructionMap[storeImm32][sizeType]); emitArgument(stackPointerReg, 4); emitArgument(reg, 4);
		emitArgument(stackOffset, 12);
	}
}
void storeRegIntoStackRegPointer(const uint8_t reg, const uint8_t addrReg, const uint32_t stackOffset, const uint8_t sizeType) {
	if (stackOffset >= 4095) {
		storeConstantInReg(scratchReg1, stackOffset);
		emitOpcode(instructionMap[storeReg32][sizeType]); emitArgument(addrReg, 4); emitArgument(reg, 4);
		emitArgument(scratchReg1, 4);
	}
	else{
		emitOpcode(instructionMap[storeImm32][sizeType]); emitArgument(addrReg, 4); emitArgument(reg, 4);
		emitArgument(stackOffset, 12);
	}
}
void storeRegisterIntoStackAbs(const uint8_t reg, const uint32_t offset) {
	storeConstantInReg(scratchReg1, offset);
	emitOpcode(strw_imm_32); emitArgument(scratchReg1, 4); emitArgument(reg, 4);
	emitArgument(0, 12);
}
forceinline void storeRegisterIntoStackRAbs(const uint8_t reg, const uint8_t reg2) {
	emitOpcode(strw_imm_32); emitArgument(reg2, 4); emitArgument(reg, 4);
	emitArgument(0, 12);
}
forceinline void storeRegisterIntoStackR(const uint8_t reg, const uint8_t reg2, const uint8_t sizeType) {
	emitOpcode(instructionMap[storeReg32][sizeType]); emitArgument(stackPointerReg, 4); emitArgument(reg, 4);
	emitArgument(reg2, 4); //kedaar was here
}
void loadRegisterFromStack(const uint8_t reg, const uint32_t stackOffset, const uint8_t sizeType) {
	if (stackOffset >= 4095) {
		emitOpcode(movw_lit_32); emitArgument(scratchReg1, 4); emitArgument(stackOffset, 16);
		emitOpcode(movt_lit_32); emitArgument(scratchReg1, 4); emitArgument(stackOffset, 16);
		emitOpcode(instructionMap[loadReg32][sizeType]); emitArgument(stackPointerReg, 4); emitArgument(reg, 4);
		emitArgument(scratchReg1, 4);
	}
	else if(stackOffset < 1021 && stackOffset % 4 == 0 && reg < 8){emitOpcode(instructionMap[loadImm16][sizeType]); emitArgument(reg, 3); emitArgument(stackOffset, 8);}
	else{
		emitOpcode(instructionMap[loadImm32][sizeType]); emitArgument(stackPointerReg, 4); emitArgument(reg, 4);
		emitArgument(stackOffset, 12);
	}
}
void loadRegisterFromStackAbs(const uint8_t reg, const uint32_t stackOffset) {
	storeConstantInReg(scratchReg1, stackOffset);
	emitOpcode(ldrw_imm_32); emitArgument(scratchReg1, 4); emitArgument(reg, 4);
	emitArgument(0, 12);
}
forceinline void loadRegisterFromStackR(const uint8_t reg, const uint8_t reg2, const uint8_t sizeType) {
	emitOpcode(instructionMap[loadReg32][sizeType]); emitArgument(stackPointerReg, 4); emitArgument(reg, 4);
	emitArgument(reg2, 4);
}
forceinline void loadRegisterFromStackRAbs(const uint8_t reg, const uint8_t reg2) {
	emitOpcode(ldrw_imm_32); emitArgument(reg2, 4); emitArgument(reg, 4);
	emitArgument(0, 12);
}
forceinline void loadRegisterFromFlags(const uint8_t reg, const uint8_t flag){
	emitOpcode(ite_32); emitFlag(flag); storeConstantInReg(reg, 1); storeConstantInReg(reg, 0);
}
forceinline void loadRegisterFromRegs(const uint8_t r1, const uint8_t r2){
	emitOpcode(mov_reg_reg_16); emitArgument(r1, 4); emitArgument(r2, 4);
}
forceinline void loadRegisterFromRegPointer(const uint8_t reg, const uint8_t spReg, const uint32_t stackOffset, const uint8_t sizeType){
	if (stackOffset >= 4095) {
		emitOpcode(movw_lit_32); emitArgument(scratchReg1, 4); emitArgument(stackOffset, 16);
		emitOpcode(movt_lit_32); emitArgument(scratchReg1, 4); emitArgument(stackOffset, 16);
		emitOpcode(instructionMap[loadReg32][sizeType]); emitArgument(spReg, 4); emitArgument(reg, 4);
		emitArgument(scratchReg1, 4);
	} else{
		emitOpcode(instructionMap[loadImm32][sizeType]); emitArgument(spReg, 4); emitArgument(reg, 4);
		emitArgument(stackOffset, 12);
	}
}
void flushRegister(uint8_t regIdx) {
	if(virtualRegFile[regIdx].dirty == locked) return;
	if(virtualRegFile[regIdx].dirty == dirty) storeRegisterIntoStack(regIdx, virtualRegFile[regIdx].stackOffset, wordSize); 
	virtualRegFile[regIdx].dirty = empty; virtualRegFile[regIdx].stackOffset = 0; // free
}
forceinline void flushRegisters(){
	for(uint8_t idx = 0; idx < maxGPRegs; idx++) flushRegister(idx);
}

forceinline uint8_t getEmptyRegister(const uint32_t stackOffset, const uint16_t priority, const uint8_t checkStack){
	uint16_t lowestPriority = UINT16_MAX, flushIdx = 0; uint8_t foundEmpty = 0;
	for(uint8_t r = 0; r < maxGPRegs; r++){
		if(virtualRegFile[r].stackOffset == stackOffset && checkStack && virtualRegFile[r].dirty != empty){flushIdx = r; goto skipEmptyFlush;}
		if(virtualRegFile[r].dirty == empty && !foundEmpty){flushIdx = r; foundEmpty = 1;}
		else if(virtualRegFile[r].priority < lowestPriority && virtualRegFile[r].dirty != locked && !foundEmpty){lowestPriority = virtualRegFile[r].priority; flushIdx = r;}
	}
	flushRegister(flushIdx); skipEmptyFlush:
	virtualRegFile[flushIdx] = (registerData){stackOffset, clean, priority};
	return flushIdx;
}

#define checkIfInRegs 1
#define noCheck 0

uint8_t movedFromStack = 0;

uint8_t moveOffsetToRegs(const uint32_t stackOffsetLoad, const uint32_t stackOffsetStore, const uint16_t priority){
	uint16_t lowestPriority = UINT16_MAX; uint8_t foundEmpty = 0;
	uint8_t flushIdx = 0; uint8_t foundLoaded = 0;
	for(uint8_t r = 0; r < maxGPRegs; r++){
		if(virtualRegFile[r].stackOffset == stackOffsetLoad && virtualRegFile[r].dirty != empty){foundLoaded = 1; flushIdx = r;} 
		else if(virtualRegFile[r].stackOffset == stackOffsetStore) virtualRegFile[r].dirty = empty;
		else if(virtualRegFile[r].dirty == empty && !foundLoaded && !foundEmpty){foundEmpty = 1; flushIdx = r;}
		else if(virtualRegFile[r].priority < lowestPriority && virtualRegFile[r].dirty != locked && !foundEmpty && !foundLoaded){lowestPriority = virtualRegFile[r].priority; flushIdx = r;}
	}
	if(!foundLoaded){
		if(!foundEmpty) flushRegister(flushIdx);
		loadRegisterFromStack(flushIdx, stackOffsetLoad, wordSize); movedFromStack = 1;
	}
	virtualRegFile[flushIdx] = (registerData){stackOffsetStore, foundLoaded ? virtualRegFile[flushIdx].dirty : clean, priority};
	return flushIdx;
}

uint8_t moveRegToRegs(const uint8_t readReg, const uint32_t stackOffsetStore, const uint32_t priority){
	uint16_t lowestPriority = UINT16_MAX; uint8_t foundEmpty = 0;
	uint8_t flushIdx = 0; uint8_t foundLoaded = 0;
	for(uint8_t r = 0; r < maxGPRegs; r++){ if(r == readReg) continue;
		else if(virtualRegFile[r].stackOffset == stackOffsetStore) virtualRegFile[r].dirty = empty;
		else if(virtualRegFile[r].dirty == empty && !foundLoaded && !foundEmpty){foundEmpty = 1; flushIdx = r;}
		else if(virtualRegFile[r].priority < lowestPriority && virtualRegFile[r].dirty != locked && !foundEmpty && !foundLoaded){lowestPriority = virtualRegFile[r].priority; flushIdx = r;}
	}
	if(!foundLoaded){
		if(!foundEmpty) flushRegister(flushIdx);
		loadRegisterFromRegs(flushIdx, readReg);
	}
	return flushIdx;
}

uint8_t moveConstantToRegs(const uint32_t val, const uint32_t stackOffsetStore, const uint16_t priority) {
	const uint8_t fIdx = getEmptyRegister(stackOffsetStore, priority, noCheck);
	storeConstantInReg(fIdx, val);
	return fIdx;
}

uint8_t moveFlagToRegs(const uint32_t stackOffsetLoad, const uint32_t stackOffsetStore, const uint8_t flag, const uint16_t priority){
	uint16_t lowestPriority = UINT16_MAX; uint8_t foundEmpty = 0;
	uint8_t flushIdx = 0; uint8_t foundLoaded = 0;
	for(uint8_t r = 0; r < maxGPRegs; r++){
		if(virtualRegFile[r].stackOffset == stackOffsetLoad && stackOffsetLoad != UINT32_MAX){foundLoaded = 1; flushIdx = r;} 
		else if(virtualRegFile[r].stackOffset == stackOffsetStore) virtualRegFile[r].dirty = empty;
		else if(virtualRegFile[r].dirty == empty && !foundEmpty){foundEmpty = 1; flushIdx = r;}
		else if(virtualRegFile[r].priority < lowestPriority && virtualRegFile[r].dirty != locked && !foundEmpty && !foundLoaded){lowestPriority = virtualRegFile[r].priority; flushIdx = r;}
	}
	if(!foundLoaded){
		if(!foundEmpty) flushRegister(flushIdx);
		if(stackOffsetLoad == virtualFlags.stackOffset) loadRegisterFromFlags(flushIdx, flag);
		else{loadRegisterFromStack(flushIdx, stackOffsetLoad, wordSize); movedFromStack = 1;}
	}
	virtualRegFile[flushIdx] = (registerData){stackOffsetStore, clean, priority};
	return flushIdx;
}
forceinline void flushFlags(){
	if(virtualFlags.dirty != empty) moveFlagToRegs(virtualFlags.stackOffset, virtualFlags.stackOffset, virtualFlags.flagType, UINT16_MAX);
}
forceinline void emptyFlags(){
	virtualFlags.dirty = empty;
}

uint8_t moveOffsetToRegsFromRegister(const uint8_t loadRegister, const uint32_t stackOffsetStore, const uint16_t priority){
	uint16_t lowestPriority = UINT16_MAX; uint8_t foundEmpty = 0; uint8_t flushIdx = 0;
	for(uint8_t r = 0; r < maxGPRegs; r++){
		if(virtualRegFile[r].stackOffset == stackOffsetStore) virtualRegFile[r].dirty = empty;
		else if(virtualRegFile[r].dirty == empty){foundEmpty = 1; flushIdx = r;}
		else if(virtualRegFile[r].priority < lowestPriority && virtualRegFile[r].dirty != locked && !foundEmpty){lowestPriority = virtualRegFile[r].priority; flushIdx = r;}
	}
	if(!foundEmpty) flushRegister(flushIdx);
	loadRegisterFromStackR(flushIdx, loadRegister, wordSize); movedFromStack = 1;
	virtualRegFile[flushIdx] = (registerData){stackOffsetStore, clean, priority};
	return flushIdx;
}

//#############################################################################################################################################
// STACK/REGISTER MANAGER

enum operandType { constant, stackVar, flagVar, nullVar };

typedef struct{
	int32_t stackOffset, size;
	const char* name; 
	uint8_t strLen; uint8_t scope;
}variableMetadata;

typedef struct operand {
	union {
		unionType value; unionType stackOffset;
	}val;
	uint32_t size;
	// address 16 gp regs
	uint8_t operandType; uint8_t flagType : 7; uint8_t forceFlush : 1; uint16_t registerPreference;
}operand;
#pragma pack(push, 1)
typedef struct{uint8_t subtype, precedence;}operator;
#pragma pack(pop)

#define maxUserVariables 256
variableMetadata variableBuffer[maxUserVariables]; uint8_t variableIdx = 0;
uint32_t curStackOffset = 0; uint32_t curCompilerTempSz = 0; uint32_t argSize = 0;
uint8_t curScope = 0;

variableMetadata addVariable(const char* name, const uint8_t strLen, const uint32_t size){
	variableBuffer[variableIdx] = (variableMetadata){curStackOffset, size, name, strLen, curScope}; 
	curStackOffset += size;
	return variableBuffer[variableIdx++];
}

forceinline void addCompilerTemp(const uint32_t sz){curStackOffset += sz; curCompilerTempSz += sz;}

forceinline uint8_t compareNames(const char* name, uint8_t len, const char* name2, uint8_t len2){
	for(; len > 0 && len2 > 0;){if(name[--len] != name2[--len2]){return 0;}}
	return (len + len2) == 0;
}

variableMetadata retrieveLocalVariable(const char* name, uint8_t len){
	for (variableMetadata* v = variableBuffer + variableIdx - 1; v >= variableBuffer; v--) {
		if (compareNames(name, len, v->name, v->strLen)) return *v; 
	}
	return (variableMetadata){0, 0, (void*)0, 0, 0};
}

//#############################################################################################################################################
// OPERATION PARSER

forceinline uint32_t evalImm(const uint32_t o1u, const uint32_t o2u, const uint8_t subtype){
	const int32_t o1 = (int)o1u, o2 = (int)o2u;
	switch(subtype){
		case opAdd: return o1 + o2;
		case opSub: return o1 - o2;
		case opMul: return o1 * o2;
		case opDiv: return o1 / o2;
		case opEqual: return o1 == o2;
		case opBitwiseAnd: return o1 & o2;
		case opBitwiseOr: return o1 | o2;
		case opBitwiseXor: return o1 ^ o2;
		case opBitwiseNot: return ~o1;
		case opCmpEqual: return o1 == o2;
	}
}

forceinline uint8_t returnImmOpcode(const uint8_t subtype, const uint16_t nt){
	switch(subtype){
		case opAdd: case opIncrement: return nt == 1 ? addw_imm_32 : adds_imm_32;
		case opSub: case opDecrement: return subw_imm_32;
		case opMul: return mulw_reg_32;
		case opBitwiseAnd: return andw_imm_32;
		case opBitwiseOr: return orrs_imm_32;
		case opBitwiseXor: return eors_imm_32;
		case opBitwiseNot: return mvn_imm_32;
		case opRightShift: return lsr_imm_32;
		case opLeftShift: return lsl_imm_32;
	}
} forceinline uint8_t returnRegOpcode(const uint8_t subtype, const uint16_t nt){
	switch(subtype){
		case opAdd: case opIncrement: return nt == 1 ? addw_reg_32 : adds_reg_32;
		case opSub: case opDecrement: return subw_reg_32;
		case opMul: return mulw_reg_32;
		case opBitwiseAnd: return andw_reg_32;
		case opBitwiseOr: return orrs_reg_32;
		case opBitwiseXor: return eors_reg_32;
		case opBitwiseNot: return mvn_reg_32;
		case opRightShift: return lsr_reg_32;
		case opLeftShift: return lsl_reg_32;
	}
}

forceinline uint8_t immSize(const uint8_t subtype, const uint16_t immSize){
	switch(subtype){
		case opBitwiseAnd: case opBitwiseXor: case opBitwiseOr: return 8;
		case opIncrement: case opDecrement: case opBitwiseNot: return 16;
		case opAdd: case opSub: return (immSize == 1) ? 12 : 8;
		case opRightShift: case opLeftShift: return 5;
	}
}

forceinline uint8_t getFlag(const uint8_t subtype){
	switch(subtype){
		case opCmpEqual: return flag_eq;
		case opCmpGreater: return flag_gt;
		case opCmpLess: return flag_lt;
		case opCmpGEqual: return flag_ge;
		case opCmpLEqual: return flag_le;
		case opCmpNEqual: return flag_ne;
	}
}

forceinline uint8_t getOppositeFlag(const uint8_t flag){
	switch(flag){
		case flag_eq: return flag_ne;
		case flag_ne: return flag_eq;
		case flag_gt: return flag_le;
		case flag_lt: return flag_ge;
		case flag_ge: return flag_lt;
		case flag_le: return flag_gt;
	}
}

forceinline uint8_t numOperands(const uint8_t subtype){
	switch(subtype){
		case opWritebackArr: return 4;
		case opWriteback: case opWritebackVolatile: case opDereferenceArr: return 3;
		case opFree: case opLogicalNot: case opBitwiseNot: case opNegate: case opReference: return 1;
		default: return 2;
	}
}

#define isTemporary(offset) (offset >= (curStackOffset - curCompilerTempSz) && offset < (curStackOffset + (curCompilerTempSz == 0)))

operand assembleOp(const operator op, const operand* operands, const uint16_t registerPermanence){
	operand ret, o1, o2;
	printf("\n");
	switch(op.subtype){
		case opEqual:{
		o2 = *operands; o1 = *(operands - 1);
		uint32_t num32BitTransfers = ((o1.size < o2.size ? o1.size : o2.size) + 3) >> 2; const uint32_t o1s = (o1.size + 3) >> 2;
		for(uint32_t wIdx = 0; wIdx < o1s; wIdx++){ movedFromStack = 0;
			if(wIdx > num32BitTransfers) {const uint8_t r = moveConstantToRegs(0, o1.val.stackOffset + wIdx * 4, registerPermanence); virtualRegFile[r].dirty = dirty; goto skipWriteback;}
			uint8_t r; switch(o2.operandType){
				case constant:
				virtualRegFile[r = moveConstantToRegs(o2.val.value, o1.val.stackOffset + wIdx * 4, registerPermanence)].dirty = dirty;
				goto skipWriteback;
				case stackVar:
				r = moveOffsetToRegs(o2.val.stackOffset + wIdx * 4, o2.val.stackOffset + wIdx * 4, registerPermanence);
				break;
				case flagVar:
				r = moveFlagToRegs(o2.val.stackOffset + wIdx * 4, o2.val.stackOffset + wIdx * 4, o2.flagType, registerPermanence);
				if(isTemporary(o2.val.stackOffset + wIdx * 4)) virtualFlags.dirty = empty;
			} if(movedFromStack) virtualRegFile[r].stackOffset = o1.val.stackOffset + wIdx * 4;
			if(!o1.forceFlush){
				if(!isTemporary(o2.val.stackOffset + wIdx * 4)){
					const uint8_t rr = getEmptyRegister(o1.val.stackOffset + wIdx * 4, registerPermanence, checkIfInRegs);
					virtualRegFile[rr].dirty = dirty; if(rr-r) loadRegisterFromRegs(rr, r);
				} else{virtualRegFile[r].dirty = dirty; 
				for(uint8_t rr = 0; rr < maxGPRegs; rr++) 
				if(virtualRegFile[rr].stackOffset == o1.val.stackOffset + wIdx * 4) virtualRegFile[rr].dirty = empty; 
				virtualRegFile[r].stackOffset = o1.val.stackOffset + wIdx * 4;}
			}
			skipWriteback:;
			if(o1.forceFlush){storeRegisterIntoStack(r, o1.val.stackOffset + wIdx * 4, wordSize); virtualRegFile[r].dirty = empty;}
		}
		return o1;
		}
		case opFree:{
		o1 = *operands; for(uint8_t r = 0; r < maxGPRegs; r++)
		if(virtualRegFile[r].stackOffset >= o1.val.stackOffset && virtualRegFile[r].stackOffset < o1.val.stackOffset + o1.size) virtualRegFile[r].dirty = empty;
		return o1;
		}
		case opReference:{
		o1 = *operands;
		return (operand){o1.val.stackOffset, 4, constant, 0, 0, UINT16_MAX};
		}
		case opDereferenceVolatile:{
		o2 = *operands; o1 = *(operands - 1);
		const uint8_t sizeType = o1.val.value == halfSz ? halfSize : (o1.val.value == byteSz ? byteSize : wordSize);
		int8_t addrReg = -1; uint8_t addrd; const uint32_t num32BitTransfers = o1.val.value >= (UINT32_MAX - 1) ? 1 : o1.val.value;
		switch(o2.operandType){
			case constant:
			for(uint8_t r = 0; r < maxGPRegs; r++) if(virtualRegFile[r].stackOffset >= o2.val.value && virtualRegFile[r].stackOffset < o2.val.value + num32BitTransfers * 4) flushRegister(r);
			addrReg = moveConstantToRegs(o2.val.value, curStackOffset - curCompilerTempSz, registerPermanence);
			addrd = empty; break;
			case stackVar: movedFromStack = 0; flushRegisters();
			addrReg = moveOffsetToRegs(o2.val.stackOffset, o2.val.stackOffset, registerPermanence);
			addrd = isTemporary(o2.val.stackOffset) ? empty : virtualRegFile[addrReg].dirty;
			virtualRegFile[addrReg].dirty = locked;
		}
		for(uint8_t r = 0; r < maxGPRegs; r++){
			if((virtualRegFile[r].stackOffset == o2.val.value && op.subtype == opDereferenceVolatile) || o2.operandType == stackVar)
			flushRegister(r);
		} uint32_t wr = num32BitTransfers; do{ wr--;
			const uint8_t rEmpty = getEmptyRegister(curStackOffset + wr * 4, registerPermanence, noCheck);
			loadRegisterFromRegPointer(rEmpty, addrReg, wr * 4, sizeType);
		}while(wr > 0);
		virtualRegFile[addrReg].dirty = addrd;
		curCompilerTempSz += num32BitTransfers * 4; curStackOffset += num32BitTransfers* 4;
		return (operand){curStackOffset - num32BitTransfers * 4, num32BitTransfers * 4, stackVar, 0, 0, registerPermanence};
		}
		case opDereference:{
		o2 = *operands; o1 = *(operands - 1);
		const uint8_t sizeType = o1.val.value == halfSz ? halfSize : (o1.val.value == byteSz ? byteSize : wordSize);
		int8_t addrReg = -1; uint8_t addrd; uint32_t constantOffset;
		uint32_t num32BitTransfers = o1.val.value >= (UINT32_MAX - 1) ? 1 : o1.val.value;
		switch(o2.operandType){
			case constant:
			constantOffset = o2.val.value; break;
			case stackVar: movedFromStack = 0;
			addrReg = moveOffsetToRegs(o2.val.stackOffset, o2.val.stackOffset, registerPermanence);
			if(num32BitTransfers == 1){
				addrd = virtualRegFile[addrReg].dirty;
				virtualRegFile[addrReg].dirty = locked;
			}
			else{
				if(!movedFromStack && !isTemporary(virtualRegFile[addrReg].stackOffset)) addrReg = moveRegToRegs(addrReg, o2.val.stackOffset, registerPermanence);
				virtualRegFile[addrReg].dirty = locked; addrd = empty;
				emitOpcode(subw_imm_32); emitArgument(addrReg, 4); emitArgument(stackPointerReg, 4); emitArgument(addrReg, 4);
			}
		}
		for(uint8_t r = 0; r < maxGPRegs; r++){
			if((virtualRegFile[r].stackOffset == o2.val.value && op.subtype == opDereferenceVolatile) || o2.operandType == stackVar)
			flushRegister(r);
		}
		do{
			num32BitTransfers--;
			switch(o2.operandType){
				case constant:
				movedFromStack = 0;
				const uint8_t r = moveOffsetToRegs(o2.val.value + num32BitTransfers * 4, o2.val.value + num32BitTransfers * 4, UINT16_MAX);
				if(movedFromStack) virtualRegFile[r].stackOffset = curStackOffset + num32BitTransfers * 4;
				else{
					const uint8_t rEmpty = getEmptyRegister(curStackOffset + num32BitTransfers * 4, registerPermanence, noCheck);
					loadRegisterFromRegs(rEmpty, r);
				}
				break;
				case stackVar:{
				const uint8_t rEmpty = getEmptyRegister(curStackOffset + num32BitTransfers * 4, registerPermanence, noCheck);
				if(num32BitTransfers-1){
					loadRegisterFromRegPointer(rEmpty, addrReg, num32BitTransfers * 4, sizeType);
				} else{
					loadRegisterFromRegs(rEmpty, addrReg);
				}
				}
			}
			
		}while(num32BitTransfers > 0);
		if(addrReg != -1){virtualRegFile[addrReg].dirty = addrd;}
		curCompilerTempSz += num32BitTransfers * 4; curStackOffset += num32BitTransfers* 4;
		return (operand){curStackOffset - num32BitTransfers * 4, num32BitTransfers * 4, stackVar, 0, 0, registerPermanence};
		}
		case opDereferenceArr:{
		int8_t addrReg = -1;  uint32_t addrConst;
		const operand o3 = *operands; o2 = *(operands - 1); o1 = *(operands - 2);
		const uint32_t num32BitTransfers = o1.val.value >= (UINT32_MAX - 1) ? 1 : o1.val.value;
		const uint8_t sizeType = o1.val.value == halfSz ? halfSize : (o1.val.value == byteSz ? byteSize : wordSize);
		uint32_t baseRdAddr = o2.val.stackOffset; int8_t offsetRegister = -1; uint8_t offd;
		switch(o3.operandType){
			case constant: baseRdAddr += o3.val.value; break;
			case stackVar: movedFromStack = 0;
			offsetRegister = moveOffsetToRegs(o3.val.stackOffset, o3.val.stackOffset, registerPermanence);
			if(!movedFromStack && !isTemporary(virtualRegFile[offsetRegister].stackOffset) && num32BitTransfers - 1){
				offsetRegister = moveRegToRegs(offsetRegister, o3.val.stackOffset, registerPermanence);
				offd = empty; emitOpcode(subw_reg_32); emitArgument(offsetRegister, 4); emitArgument(stackPointerReg, 4); emitArgument(offsetRegister, 4);
			}else offd = virtualRegFile[offsetRegister].dirty;
			virtualRegFile[offsetRegister].dirty = locked;
		}const uint32_t offsetCheck = curStackOffset + (curCompilerTempSz == 0);
		for(uint32_t wIdx = 0; wIdx < num32BitTransfers; wIdx++){
			
			if(num32BitTransfers - 1){
				uint8_t readReg = getEmptyRegister(curStackOffset + wIdx * 4, registerPermanence, noCheck);
				if(offsetRegister != -1){
					loadRegisterFromRegPointer(readReg, offsetRegister, wIdx * 4, sizeType);
				}else{
					loadRegisterFromStack(readReg, baseRdAddr + wIdx * 4, sizeType);
				}
			} else{
				if(offsetRegister != -1){
					uint8_t readReg = getEmptyRegister(curStackOffset + wIdx * 4, registerPermanence, noCheck);
					loadRegisterFromStackR(readReg, offsetRegister, sizeType);
				}else{
					movedFromStack = 0;
					const uint8_t r = moveOffsetToRegs(baseRdAddr + wIdx * 4, baseRdAddr + wIdx * 4, registerPermanence);
					if(movedFromStack) virtualRegFile[r].stackOffset = curStackOffset + wIdx * 4;
					else{
						const uint8_t rEmpty = getEmptyRegister(curStackOffset + wIdx * 4, registerPermanence, noCheck);
						loadRegisterFromRegs(rEmpty, r);
					}
				}
			}
		}virtualRegFile[offsetRegister].dirty = offd;
		curStackOffset += num32BitTransfers << 2; curCompilerTempSz += num32BitTransfers << 2;
		return (operand){curStackOffset - num32BitTransfers * 4, num32BitTransfers * 4, stackVar, 0, 0, registerPermanence};
		}
		case opWriteback: case opWritebackVolatile:{
		int8_t addrReg = -1;  uint32_t addrConst; uint8_t addrd = empty;
		const operand o3 = *operands; o2 = *(operands - 1); o1 = *(operands - 2);
		const uint8_t sizeType = o1.val.value == halfSz ? halfSize : (o1.val.value == byteSz ? byteSize : wordSize);
		const uint32_t num32BitTransfers = o1.val.value >= (UINT32_MAX - 1) ? 1 : o1.val.value;
		switch(o2.operandType){
			case constant:
			for(uint8_t r = 0; r < maxGPRegs; r++) if(virtualRegFile[r].stackOffset >= o2.val.stackOffset + o3.val.value && virtualRegFile[r].stackOffset < o2.val.stackOffset + o3.val.value + num32BitTransfers* 4) 
			virtualRegFile[r].dirty = empty; addrConst = o2.val.value; 
			if(op.subtype == opWritebackVolatile) addrReg = moveConstantToRegs(o2.val.value, curStackOffset - curCompilerTempSz, registerPermanence); break;
			case stackVar:
			movedFromStack = 0;
			addrReg = moveOffsetToRegs(o2.val.stackOffset, o2.val.stackOffset, registerPermanence);
			if(num32BitTransfers == 1 || op.subtype == opWritebackVolatile){
				addrd = virtualRegFile[addrReg].dirty;
				virtualRegFile[addrReg].dirty = locked;
			}
			else{
				if(!movedFromStack && !isTemporary(virtualRegFile[addrReg].stackOffset)) addrReg = moveRegToRegs(addrReg, o2.val.stackOffset, registerPermanence);
				virtualRegFile[addrReg].dirty = locked; addrd = empty;
				emitOpcode(subw_imm_32); emitArgument(addrReg, 4); emitArgument(stackPointerReg, 4); emitArgument(addrReg, 4);
			}
			flushRegisters();
		}const uint32_t offsetCheck = curStackOffset + (curCompilerTempSz == 0);
		for(uint32_t wIdx = 0; wIdx < num32BitTransfers; wIdx++){
			uint8_t readReg, rdrd = empty;
			if(wIdx >= (o3.size + 3) >> 2){readReg = moveConstantToRegs(0, UINT32_MAX, UINT16_MAX); virtualRegFile[readReg].dirty = empty;}
			else switch(o3.operandType){
				case constant:
				readReg = moveConstantToRegs(o3.val.value, UINT32_MAX, UINT16_MAX); virtualRegFile[readReg].dirty = empty;
				break;
				case stackVar:
				readReg = moveOffsetToRegs(o3.val.stackOffset + wIdx * 4, o3.val.stackOffset + wIdx * 4, o3.registerPreference);
				if(!isTemporary(o3.val.stackOffset + wIdx * 4)) rdrd = virtualRegFile[readReg].dirty;
				break;
			}
			if(addrReg > -1){
				if(num32BitTransfers-1 || op.subtype == opWritebackVolatile){
					storeRegIntoStackRegPointer(readReg, addrReg, wIdx * 4, sizeType);
				}else{
					storeRegisterIntoStackR(readReg, addrReg, sizeType);
				}
			}
			else storeRegisterIntoStack(readReg, addrConst + wIdx * 4, sizeType);
			if(isTemporary(virtualRegFile[readReg].stackOffset) || o3.operandType == constant) virtualRegFile[readReg].dirty = empty;
		}
		if(addrReg > -1) virtualRegFile[addrReg].dirty = addrd;
		curStackOffset += num32BitTransfers * 4; curCompilerTempSz += num32BitTransfers * 4;
		return (operand){curStackOffset - num32BitTransfers * 4, num32BitTransfers * 4, stackVar, 0, 0, registerPermanence};
		}
		case opWritebackArr:{
		int8_t addrReg = -1;  uint32_t addrConst;
		const operand o4 = *operands; const operand o3 = *(operands - 1); o2 = *(operands - 2); o1 = *(operands - 3);
		const uint8_t sizeType = o1.val.value == halfSz ? halfSize : (o1.val.value == byteSz ? byteSize : wordSize);
		const uint32_t num32BitTransfers = o1.val.value >= (UINT32_MAX - 1) ? 1 : o1.val.value;
		uint32_t baseWbAddr = o2.val.stackOffset; int8_t offsetRegister = -1; uint8_t offd;
		switch(o3.operandType){
			case constant: baseWbAddr += o3.val.value; 
			for(uint8_t r = 0; r < maxGPRegs; r++) if(virtualRegFile[r].stackOffset >= o2.val.stackOffset + o3.val.value && virtualRegFile[r].stackOffset < o2.val.stackOffset + o3.val.value + num32BitTransfers * 4) 
			virtualRegFile[r].dirty = empty; offd = empty; break;
			case stackVar: for(uint8_t r = 0; r < maxGPRegs; r++) if(virtualRegFile[r].stackOffset >= o2.val.stackOffset && virtualRegFile[r].stackOffset < o2.val.stackOffset + num32BitTransfers * 4) flushRegister(r);
			movedFromStack = 0; offsetRegister = moveOffsetToRegs(o3.val.stackOffset, o3.val.stackOffset, registerPermanence); 
			if(!movedFromStack && !isTemporary(virtualRegFile[offsetRegister].stackOffset) && num32BitTransfers - 1) {offsetRegister = moveRegToRegs(offsetRegister, o3.val.stackOffset, registerPermanence);
			offd = empty; 
			emitOpcode(subw_reg_32); emitArgument(offsetRegister, 4); emitArgument(stackPointerReg, 4); emitArgument(offsetRegister, 4);
			}else offd = virtualRegFile[offsetRegister].dirty;
			virtualRegFile[offsetRegister].dirty = locked;
		}const uint32_t offsetCheck = curStackOffset + (curCompilerTempSz == 0);
		for(uint32_t wIdx = 0; wIdx < num32BitTransfers; wIdx++){
			uint8_t readReg; switch(o4.operandType){
				case constant: readReg = moveConstantToRegs(wIdx ? 0 : o4.val.value, curStackOffset - curCompilerTempSz, UINT16_MAX); break;
				case stackVar: if(wIdx >= (o4.val.stackOffset + 3) >> 2){readReg = moveConstantToRegs(0, curStackOffset - curCompilerTempSz, UINT16_MAX); break;} 
			readReg = moveOffsetToRegs(o4.val.value + wIdx * 4, o4.val.value + wIdx * 4, registerPermanence); break;
			} if(num32BitTransfers - 1){
				if(offsetRegister != -1){
					storeRegIntoStackRegPointer(readReg, offsetRegister, wIdx * 4, sizeType);
				}else{
					storeRegisterIntoStack(readReg, baseWbAddr + wIdx * 4, sizeType);
				}
			} else{
				if(offsetRegister != -1){
					storeRegisterIntoStackR(readReg, offsetRegister, sizeType);
				}else{
					storeRegisterIntoStack(readReg, baseWbAddr, sizeType);
				}	
			}if(virtualRegFile[readReg].stackOffset >= curStackOffset - curCompilerTempSz && virtualRegFile[readReg].stackOffset < offsetCheck) virtualRegFile[readReg].dirty = empty;
		}virtualRegFile[offsetRegister].dirty = offd;
		curStackOffset += num32BitTransfers << 2; curCompilerTempSz += num32BitTransfers << 2;
		for(uint8_t r = 0; r < maxGPRegs; r++){
			if(virtualRegFile[r].stackOffset >= baseWbAddr && virtualRegFile[r].stackOffset < baseWbAddr + num32BitTransfers * 4) virtualRegFile[r].dirty = empty;
		}
		return (operand){curStackOffset - num32BitTransfers * 4, num32BitTransfers * 4, stackVar, 0, 0, registerPermanence};
		}
		case opSub: case opAdd: case opBitwiseAnd: case opBitwiseOr: case opBitwiseXor: case opRightShift: case opLeftShift:{
		o2 = *operands, o1 = *(operands - 1);
		uint8_t cbi = addBlock();
		const uint32_t num32BitAdds = ((o1.size > o2.size ? o1.size : o2.size) + 3) >> 2;
		for(uint32_t wIdx = 0; wIdx < num32BitAdds; wIdx++){
			int64_t constantInAdd = -1; int8_t r1 = -1; uint8_t r1d, r2d;
			switch(o1.operandType){
				case constant:
				constantInAdd = wIdx ? 0 : o1.val.value;
				if(o1.val.value >= 2 << immSize(op.subtype, num32BitAdds) && o2.operandType != constant && !wIdx) r1 = moveConstantToRegs(o1.val.value, curStackOffset - curCompilerTempSz, UINT16_MAX);
				break;
				case stackVar:
				r1 = moveOffsetToRegs(o1.val.stackOffset + wIdx * 4, o1.val.stackOffset + wIdx * 4, registerPermanence);
				r1d = virtualRegFile[r1].dirty;
				break;
			} if(r1 != -1) virtualRegFile[r1].dirty = locked;
			int8_t r2 = -1;
			switch(o2.operandType){
				case constant:
				if(constantInAdd >= 0) {return (operand) {evalImm(constantInAdd, o2.val.value, op.subtype), 4, constant, 0, 0, registerPermanence};}
				if(!wIdx){constantInAdd = o2.val.value; if(o2.val.value >= 2 << immSize(op.subtype, num32BitAdds)) r2 = moveConstantToRegs(o2.val.value, curStackOffset - curCompilerTempSz, UINT16_MAX);}
				else constantInAdd = 0;
				break;
				case stackVar:
				if(r1 == -1){
					r1 = moveOffsetToRegs(o2.val.stackOffset + wIdx * 4, o2.val.stackOffset + wIdx * 4, registerPermanence);
					r1d = virtualRegFile[r1].dirty; virtualRegFile[r1].dirty = locked;
				} else {
					r2 = moveOffsetToRegs(o2.val.stackOffset + wIdx * 4, o2.val.stackOffset + wIdx * 4, registerPermanence);
					r2d = virtualRegFile[r2].dirty; virtualRegFile[r2].dirty = locked;
				}
			}const uint32_t compilerSz = curStackOffset + (curCompilerTempSz == 0);
			if(virtualRegFile[r1].stackOffset >= curStackOffset - curCompilerTempSz && virtualRegFile[r1].stackOffset < compilerSz)
			virtualRegFile[r1].dirty = empty; else virtualRegFile[r1].dirty = r1d;
			if(r2 != -1 && virtualRegFile[r2].stackOffset >= curStackOffset - curCompilerTempSz && virtualRegFile[r2].stackOffset < compilerSz)
			virtualRegFile[r2].dirty = empty; else virtualRegFile[r2].dirty = r2d;
			const uint8_t resultReg = getEmptyRegister(curStackOffset + wIdx * 4, registerPermanence, noCheck); virtualRegFile[resultReg].dirty = dirty;
			if(r2 == -1) emitOpcode(returnImmOpcode(op.subtype, num32BitAdds)); else emitOpcode(returnRegOpcode(op.subtype, num32BitAdds));
			if(op.subtype == opAdd || op.subtype == opSub) emitModifier(wIdx == 0);
			emitArgument(resultReg, 4); emitArgument(r1, 4); emitArgument(r2 == -1 ? constantInAdd : r2, r2 == -1 ? immSize(op.subtype, num32BitAdds) : 4);
		}
		curStackOffset += num32BitAdds * 4; curCompilerTempSz += num32BitAdds * 4;
		endBlock(cbi);
		return (operand){curStackOffset - num32BitAdds * 4, num32BitAdds * 4, stackVar, 0, 0, registerPermanence};
		}
		case opBitwiseNot:{
		o1 = *operands;
		const uint32_t num32BitInvs = (o1.size + 3) >> 2;
		switch(o1.operandType){
			case constant: return (operand){evalImm(o1.val.value, 0, op.subtype), 4, constant, 0, 0, registerPermanence};
			case stackVar:
			for(uint32_t wIdx = 0; wIdx < num32BitInvs; wIdx++){
				const uint8_t r1 = moveOffsetToRegs(o1.val.stackOffset + wIdx * 4, o1.val.stackOffset + wIdx * 4, registerPermanence); uint8_t r1d;
				if(isTemporary(virtualRegFile[r1].stackOffset)) virtualRegFile[r1].dirty = empty;
				else{r1d = virtualRegFile[r1].dirty; virtualRegFile[r1].dirty = locked;}
				const uint8_t resultReg = getEmptyRegister(curStackOffset + wIdx * 4, registerPermanence, noCheck);
				virtualRegFile[r1].dirty = r1d; virtualRegFile[resultReg].dirty = dirty;
				emitOpcode(mvn_reg_32); emitArgument(resultReg, 4); emitArgument(r1, 4);
			}
		}
		curStackOffset += num32BitInvs * 4; curCompilerTempSz += num32BitInvs * 4;
		return (operand){curStackOffset - num32BitInvs * 4, num32BitInvs * 4, stackVar, 0, 0, registerPermanence};
		}
		case opIncrement: case opDecrement:{
		o2 = *operands, o1 = *(operands - 1);
		uint8_t cbi = addBlock();
		const uint32_t num32BitAdds = ((o1.size > o2.size ? o1.size : o2.size) + 3) >> 2;
		for(uint32_t wIdx = 0; wIdx < num32BitAdds; wIdx++){
			int64_t constantInAdd = -1; int8_t r1 = -1; uint8_t r1d, r2d;
			r1 = moveOffsetToRegs(o1.val.stackOffset + wIdx * 4, o1.val.stackOffset + wIdx * 4, registerPermanence);
			r1d = virtualRegFile[r1].dirty; virtualRegFile[r1].dirty = locked; int8_t r2 = -1;
			switch(o2.operandType){
				case constant:
				if(!wIdx){constantInAdd = o2.val.value; if(o2.val.value >= 2 << immSize(op.subtype, num32BitAdds)) r2 = moveConstantToRegs(o2.val.value, curStackOffset - curCompilerTempSz, UINT16_MAX);}
				else constantInAdd = 0;
				break;
				case stackVar:
				if(r1 == -1){
					r1 = moveOffsetToRegs(o2.val.stackOffset + wIdx * 4, o2.val.stackOffset + wIdx * 4, o2.registerPreference);
					r1d = virtualRegFile[r1].dirty; virtualRegFile[r1].dirty = locked;
				} else {
					r2 = moveOffsetToRegs(o2.val.stackOffset + wIdx * 4, o2.val.stackOffset + wIdx * 4, o2.registerPreference);
					r2d = virtualRegFile[r2].dirty; virtualRegFile[r2].dirty = locked;
				}
			}const uint32_t compilerSz = curStackOffset + (curCompilerTempSz == 0); virtualRegFile[r1].dirty = dirty;
			if(r2 != -1 && virtualRegFile[r2].stackOffset >= curStackOffset - curCompilerTempSz && virtualRegFile[r2].stackOffset < compilerSz)
			virtualRegFile[r2].dirty = empty; else virtualRegFile[r2].dirty = r2d;
			if(r2 == -1) emitOpcode(returnImmOpcode(op.subtype, num32BitAdds)); else emitOpcode(returnRegOpcode(op.subtype, num32BitAdds));
			emitModifier(wIdx == 0);
			emitArgument(r1, 4); emitArgument(r1, 4); emitArgument(r2 == -1 ? constantInAdd : r2, r2 == -1 ? immSize(op.subtype, num32BitAdds) : 4);
		}
		curStackOffset += num32BitAdds * 4; curCompilerTempSz += num32BitAdds * 4;
		endBlock(cbi);
		return (operand){curStackOffset - num32BitAdds * 4, num32BitAdds * 4, stackVar, 0, 0, registerPermanence};
		}
		case opCmpEqual: case opCmpGreater: case opCmpLess: case opCmpGEqual: case opCmpLEqual: case opCmpNEqual:{
		o2 = *operands, o1 = *(operands - 1);
		uint8_t cbi = addBlock();
		flushFlags();
		virtualFlags.flagType = op.subtype; virtualFlags.dirty = dirty;
		if(o1.operandType == constant && o2.operandType == constant) return (operand) {4, 4, constant, 0, 0, registerPermanence};
		const uint32_t num32BitCmps = ((o1.size > o2.size ? o1.size : o2.size) + 3) >> 2;
		for(uint32_t wIdx = 0; wIdx < num32BitCmps; wIdx++){
			int8_t r1 = -1; uint32_t constantInCmp = 0; uint8_t r1d, r2d;
			switch(o1.operandType){
				case constant:
				if(!wIdx){constantInCmp = o1.val.value; if(o1.val.value >= UINT8_MAX) r1 = moveConstantToRegs(o1.val.value, curStackOffset - curCompilerTempSz, UINT16_MAX);}
				else constantInCmp = 0;
				break;
				case stackVar:
				r1 = moveOffsetToRegs(o1.val.stackOffset + wIdx * 4, o1.val.stackOffset + wIdx * 4, o1.registerPreference);
				r1d = virtualRegFile[r1].dirty;
				break;
			} if(r1 != -1) virtualRegFile[r1].dirty = locked;
			int8_t r2 = -1;
			switch(o2.operandType){
				case constant:
				if(!wIdx){constantInCmp = o2.val.value; if(o2.val.value >= UINT8_MAX) r2 = moveConstantToRegs(o2.val.value, curStackOffset - curCompilerTempSz, UINT16_MAX);}
				else constantInCmp = 0;
				break;
				case stackVar:
				if(r1 == -1){
					r1 = moveOffsetToRegs(o2.val.stackOffset + wIdx * 4, o2.val.stackOffset + wIdx * 4, o2.registerPreference);
					r1d = virtualRegFile[r1].dirty; virtualRegFile[r1].dirty = locked;
				}else{
					r2 = moveOffsetToRegs(o2.val.stackOffset + wIdx * 4, o2.val.stackOffset + wIdx * 4, o2.registerPreference);
					r2d = virtualRegFile[r2].dirty;  virtualRegFile[r2].dirty = locked;
				}
			} const uint32_t compilerCmpOffset = curStackOffset + (curCompilerTempSz == 0);
			if(virtualRegFile[r1].stackOffset >= curStackOffset - curCompilerTempSz && virtualRegFile[r1].stackOffset < compilerCmpOffset)
			virtualRegFile[r1].dirty = empty; else virtualRegFile[r1].dirty = r1d;
			if(r2 != -1){ if(virtualRegFile[r2].stackOffset >= curStackOffset - curCompilerTempSz && virtualRegFile[r2].stackOffset < compilerCmpOffset)
			virtualRegFile[r2].dirty = empty; else virtualRegFile[r2].dirty = r2d;}
			if(r2 == -1) emitOpcode(subs_imm_32); else emitOpcode(subs_reg_32); emitModifier(wIdx == 0); emitArgument(scratchReg1, 4);
			emitArgument(r1, 4); emitArgument(r2 == -1 ? constantInCmp : r2, r2 == -1 ? 12 : 4);
		}
		virtualFlags.flagType = getFlag(op.subtype);
		virtualFlags.stackOffset = curStackOffset; curStackOffset += 4; curCompilerTempSz += 4;
		endBlock(cbi);
		return (operand){curStackOffset - 4, 4, flagVar, getFlag(op.subtype), 0, registerPermanence};
		}
		case opLogicalAnd: case opLogicalOr:{
		o2 = *operands, o1 = *(operands - 1);
		int8_t lazyFlag = -1; int8_t r1 = -1; int8_t r2 = -1; int8_t constantResult = 0;
		uint8_t r1d;
		switch(o1.operandType){
			case flagVar: if(virtualFlags.stackOffset == o1.val.stackOffset){lazyFlag = o1.flagType; break;}
			case stackVar: r1 = moveOffsetToRegs(o1.val.stackOffset, o1.val.stackOffset, registerPermanence); r1d = virtualRegFile[r1].dirty; virtualRegFile[r1].dirty = locked; break;
			case constant: constantResult += op.subtype == opLogicalAnd ? (o1.val.value == 0 ? -2 : 1) : (o1.val.value == 0 ? 1 : -2); break;
		} switch(o2.operandType){
			case flagVar: if(virtualFlags.stackOffset == o2.val.stackOffset){lazyFlag = o2.flagType; break;}
			case stackVar: if(r1 != -1) r2 = moveOffsetToRegs(o2.val.stackOffset, o2.val.stackOffset, registerPermanence); break;
			r1 = moveOffsetToRegs(o2.val.stackOffset, o2.val.stackOffset, registerPermanence); break;
			case constant: constantResult += op.subtype == opLogicalAnd ? (o2.val.value == 0 ? -2 : 1) : (o2.val.value == 0 ? 1 : -2); break;
		}
		if(constantResult < 0) return (operand){op.subtype == opLogicalOr, 4, constant, 0, 0, registerPermanence};
		else if(constantResult > (op.subtype == opLogicalAnd)) return (operand){op.subtype != opLogicalOr, 4, constant, 0, 0, registerPermanence};
		if(lazyFlag != -1){emitOpcode(it_32); emitFlag(op.subtype == opLogicalAnd ? lazyFlag : getOppositeFlag(lazyFlag));}
		if(r1 != -1){
			emitOpcode(cmp_imm_32); emitArgument(r1, 4); emitArgument(0, 12);
			emitOpcode(it_32); emitFlag(op.subtype == opLogicalAnd ? flag_ne : flag_eq);
			virtualRegFile[r1].dirty = isTemporary(virtualRegFile[r1].stackOffset) ? empty : r1d;
		}
		if(r2 != -1){
			emitOpcode(cmp_imm_32); emitArgument(r2, 4); emitArgument(0, 12);
			virtualRegFile[r2].dirty = isTemporary(virtualRegFile[r2].stackOffset) ? empty : virtualRegFile[r2].dirty;
		}
		virtualFlags.dirty = dirty; virtualFlags.stackOffset = curStackOffset; virtualFlags.flagType = flag_ne;
		curStackOffset += 4; curCompilerTempSz += 4;
		return (operand){curStackOffset - 4, 4, flagVar, flag_ne, 0, registerPermanence};
		}
		case opNegate:{
		o1 = *(operands); o2 = (operand){-1, 4, constant, 0, 0, registerPermanence}; goto skpInitO;
		case opMul: case opDiv:
		o1 = *(operands-1); o2 = *operands; skpInitO:;
		int8_t r1 = -1, r2 = -1; int64_t constantInMul = -1; uint8_t r1d, r2d;
		switch(o1.operandType){
			case constant: constantInMul = o1.val.value; break;
			case stackVar: r1 = moveOffsetToRegs(o1.val.stackOffset, o1.val.stackOffset, registerPermanence);
			r1d = virtualRegFile[r1].dirty; virtualRegFile[r1].dirty = locked;
		} switch(o2.operandType){
			case constant: if(constantInMul != -1) return (operand){evalImm(constantInMul, o2.val.value, op.subtype), 4, constant, 0, 0, registerPermanence};
			constantInMul = o2.val.value; break;
			case stackVar: 
			if(r1 == -1){
				r1 = moveOffsetToRegs(o2.val.stackOffset, o2.val.stackOffset, registerPermanence);
				r1d = virtualRegFile[r1].dirty; virtualRegFile[r1].dirty = locked;
			}
			else{
				r2 = moveOffsetToRegs(o2.val.stackOffset, o2.val.stackOffset, registerPermanence);
				r2d = virtualRegFile[r2].dirty;
			}
		}
		const uint32_t stackOffCheck = curStackOffset + (curCompilerTempSz == 0);
		if(constantInMul != -1) r2 = moveConstantToRegs(constantInMul, curStackOffset - curCompilerTempSz, UINT16_MAX);
		if(virtualRegFile[r2].stackOffset >= curStackOffset - curCompilerTempSz && virtualRegFile[r2].stackOffset < stackOffCheck) virtualRegFile[r2].dirty = empty;
		else virtualRegFile[r2].dirty = r2d;
		if(virtualRegFile[r1].stackOffset >= curStackOffset - curCompilerTempSz && virtualRegFile[r1].stackOffset < stackOffCheck) virtualRegFile[r1].dirty = empty;
		else virtualRegFile[r1].dirty = r1d;
		const uint8_t resultReg = getEmptyRegister(curStackOffset, registerPermanence, noCheck); virtualRegFile[resultReg].dirty = dirty;
		emitOpcode(op.subtype == opMul ? mulw_reg_32 : divw_reg_32); emitArgument(resultReg, 4); emitArgument(r1, 4); emitArgument(r2, 4);
		curStackOffset += 4; curCompilerTempSz += 4;
		return (operand){curStackOffset - 4, 4, stackVar, 0, 0, registerPermanence};	
		}
		case opScaleM: case opScaleD:{
		o2 = *operands, o1 = *(operands - 1);
		int8_t r1 = -1, r2 = -1; int64_t constantInMul = -1; uint8_t r1d, r2d;
		r1 = moveOffsetToRegs(o1.val.stackOffset, o1.val.stackOffset, registerPermanence);
		r1d = virtualRegFile[r1].dirty; virtualRegFile[r1].dirty = locked;
		switch(o2.operandType){
			case constant: r2 = moveConstantToRegs(o2.val.value, UINT32_MAX, UINT16_MAX); r2d = empty; break;
			case stackVar: 
			r2 = moveOffsetToRegs(o2.val.stackOffset, o2.val.stackOffset, registerPermanence);
			r2d = virtualRegFile[r2].dirty; 
		} virtualRegFile[r2].dirty = locked;
		const uint32_t stackOffCheck = curStackOffset + (curCompilerTempSz == 0);
		if(virtualRegFile[r2].stackOffset >= curStackOffset - curCompilerTempSz && virtualRegFile[r2].stackOffset < stackOffCheck) virtualRegFile[r2].dirty = empty;
		virtualRegFile[r1].dirty = dirty; if(virtualRegFile[r2].dirty == locked) virtualRegFile[r2].dirty = r2d;
		emitOpcode(op.subtype == opScaleM ? mulw_reg_32 : divw_reg_32); emitArgument(r1, 4); emitArgument(r1, 4); emitArgument(r2, 4);
		curStackOffset += 4;
		return (operand){curStackOffset - 4, 4, stackVar, 0, 0, registerPermanence};	
		}
	}
}

//#############################################################################################################################################
// EXPRESSION PARSER

#define maxOperatorDepth 64
#define maxOperands 128
#define maxBranchDepth 32

// comments for clarity here, sorry for the confusion with precedences.
const uint8_t operatorPrecedence[] = { 
    4, 4, 1, 1, 5, 1, 5, 1, 6, // Add, Sub, Inc, Dec, Mul, scaleMul, Div, scaleDiv, negate
    5, 5, 1, 1,          // Right Shift, Left Shift, Equal (Assignment), Free (Manual register management shi)
    8, 1, 1, 8, 8,    // Reference(Unary), Writeback, Dereference, Array Writeback, Array Dereference
    4, 4, 8, 4, // Bitwise: Or, And, Not(Unary), Xor
    2, 2, 8,    // Logical: And, Or, Not(Unary)
    3, 3, 3, 3, 3, 3, // Comparisons: Greater, Less, Equal, GEqual, NEqual, LEqual
    8, 1     // Volatile Deref/Writeback
};

typedef struct{
	uint32_t registerStatus[maxGPRegs];
}registerSnapshot;

forceinline registerSnapshot getSnapshot(){
	registerSnapshot ret;
	#pragma unroll
	for(uint8_t r = 0; r < maxGPRegs; r++){
		ret.registerStatus[r] = virtualRegFile[r].dirty == empty ? UINT32_MAX : virtualRegFile[r].stackOffset;
	} return ret;
}
forceinline void restoreSnapshot(const registerSnapshot snapshot){
	for(uint8_t r = 0; r < maxGPRegs; r++){
		if(snapshot.registerStatus[r] != UINT32_MAX){
			for(uint8_t r2 = 0; r2 < maxGPRegs; r2++) if(virtualRegFile[r2].stackOffset == snapshot.registerStatus[r] && virtualRegFile[r2].dirty != empty){
				if(r2 != r){flushRegister(r); loadRegisterFromRegs(r, r2); virtualRegFile[r].dirty = virtualRegFile[r2].dirty; virtualRegFile[r2].dirty = empty;} goto continueLoop; 
			}
			flushRegister(r); loadRegisterFromStack(r, snapshot.registerStatus[r], wordSize); virtualRegFile[r].dirty = clean;
		} else flushRegister(r);
		continueLoop:;
	}
}
operator operatorStack[maxOperatorDepth]; operand operandStack[maxOperands];

forceinline void backpatch(const uint32_t addressToPatch, const uint32_t memoryOffset, uint8_t* progOrigin){
	const uint32_t trueJump = memoryOffset - addressToPatch - 4;
	printf("BACKPATCH %d\n", trueJump);
	uint8_t *p = &progOrigin[addressToPatch];
	const uint16_t tj = trueJump >> 1; p[2] |= tj & 0xFF; 
	p[3] |= (tj >> 8) & 0b11; p[0] |= (tj >> 10) & 0b11111;
}

forceinline void backpatch_b_imm_32(uint32_t addr, uint32_t target, uint8_t* prog) {
	int32_t v = (target - addr - 4) >> 1; uint32_t s = (v >> 23) & 1;
	uint32_t j1 = ((v >> 22) ^ s ^ 1) & 1, j2 = ((v >> 21) ^ s ^ 1) & 1; uint8_t *p = &prog[addr];
	p[0] = (v >> 11) & 0xFF; p[1] = 0xF0 | (s << 2) | ((v >> 19) & 3);
	p[2] = v & 0xFF; p[3] = 0x90 | (j1 << 5) | (j2 << 3) | ((v >> 8) & 7);
}

#define branchKeywordLimit 16
typedef struct{
	uint32_t jumpbackAddr, backpatchAddr;
	registerSnapshot snapshot;
	uint32_t breakAddrs[branchKeywordLimit]; uint8_t numBreaks;
	uint32_t continueAddrs[branchKeywordLimit]; uint8_t numContinues, cbi;
}relativeLoopOffset;
#define emptyLoopOffset (relativeLoopOffset){.numBreaks = 0}
typedef struct{
	uint32_t jumpbackAddr; registerSnapshot snapshot;
	uint8_t cbi;
}relativeIfOffset;
relativeLoopOffset relativeLoopBlocks[maxBranchDepth]; uint8_t relativeLoopIdx;
relativeIfOffset relativeIfBlocks[maxBranchDepth]; uint8_t relativeIfIdx;

void decrementScope(){
	curScope--; if (variableIdx == 0) { goto stripTemporaries; }
	for (variableMetadata* v = variableBuffer + variableIdx - 1; v >= variableBuffer && v->scope > curScope; v--) {
		variableIdx--;
		curStackOffset -= v->size;
	} stripTemporaries:;
	curStackOffset -= curCompilerTempSz; curCompilerTempSz = 0;
}
forceinline void decrementScopeKey(const uint8_t keyType){
	decrementScope(); switch(keyType){
	case ifKey: case elseKey: relativeIfIdx--; break;
	case whileKey: relativeLoopIdx--;
	}
}

forceinline void incrementScope(){curScope++;}
forceinline void incrementScopeKey(const uint8_t keyType, const uint8_t cbi){
	incrementScope(); switch(keyType){
	case ifKey: case elseKey: relativeIfBlocks[relativeIfIdx].cbi = cbi; relativeIfIdx++; break;
	case whileKey: relativeLoopBlocks[relativeLoopIdx].cbi = cbi; relativeLoopIdx++;
	}
}

#pragma pack(push, 1)
typedef struct{
	uint8_t foundAllocation : 1;
	uint8_t foundFlush : 1;
	uint8_t foundVolatile : 1;
	uint8_t foundNegation : 1;
	uint8_t foundArgument : 1;
}persistentToken;
#pragma pack(pop, 1)

uint8_t branchType[maxBranchDepth]; uint8_t branchDepth = 0;
uint8_t operatorIdx, operandIdx;

forceinline uint8_t combineOperators(const uint8_t st1, const uint8_t st2){
	switch(st2){
		case opEqual:
		switch(st1){
			case opDereference: return opWriteback; break;
			case opDereferenceArr: return opWritebackArr; break;
			case opDereferenceVolatile: return opWritebackVolatile; break;
			case opAdd: return opIncrement; break;
			case opSub: return opDecrement; break;
			case opMul: return opScaleM; break;
			case opDiv: return opScaleD; break;
		}
		break;
		case opCmpEqual:
		switch(st1){
			case opLogicalNot: return opCmpNEqual; break;
			case opCmpGreater: return opCmpGEqual; break;
			case opCmpLess: return opCmpLEqual; break;
		} break;
	}return st1;
}

forceinline uint8_t flushOperatorStacks(const uint32_t registerPermanence){
	for(int8_t idx = operatorIdx - 1; idx >= 0; idx--){
		if(numOperands(operatorStack[idx].subtype) > operandIdx){return unexpectedExpression;}
		const operand tmp = assembleOp(operatorStack[idx], operandStack + operandIdx - 1, registerPermanence);
		operandIdx -= numOperands(operatorStack[idx].subtype);
		operandStack[operandIdx++] = tmp;
	}
	if(operandIdx > 1) return unexpectedExpression; operatorIdx = 0; return noError;
}
forceinline void clearCompilerTemporaries(){
	for(uint8_t r = 0; r < maxGPRegs; r++){
		if(virtualRegFile[r].stackOffset > curStackOffset - curCompilerTempSz && virtualRegFile[r].stackOffset < curStackOffset){
			virtualRegFile[r].dirty = empty;
		}
	}
	curStackOffset -= curCompilerTempSz; curCompilerTempSz = 0;
}
uint8_t assembleSource(const char* src, uint8_t* progOrigin){
	initializeVirtualFlags(); initializeVirtualRegs(); curScope = 0; progAddr = 0, progAddrC = 0;
	setSource(src); int8_t branchTypeFound = -1; outputBuf = progOrigin;
	operatorIdx = 0; operandIdx = 0; relativeLoopIdx = 0; relativeIfIdx = 0;
	uint32_t registerPermanence = 0, allocationSz = 0; 
	persistentToken persistentTokens; token previousToken = (token){(void*)0, 0, nullToken, 0}; uint16_t precedence = 0;
	*((uint8_t*)&persistentTokens) = 0;
	uint8_t refreshToken = 1;
	do{if(refreshToken) nextToken(); else refreshToken = 1;
		switch(curToken.type){
		case constantToken:
		persistentTokens.foundAllocation = 0;
		uint32_t literal = 0;
		for(uint8_t digit = 0; digit < curToken.len; digit++){
			literal *= 10;
			literal += curToken.str[digit] - '0';
		}
		if(previousToken.type == sizeToken){literal *= 4; persistentTokens.foundAllocation = 1; allocationSz = literal;}
		operandStack[operandIdx++] = (operand){literal, 4, constant, 0, 0, registerPermanence};
		break;
		case sizeToken:
		persistentTokens.foundAllocation = 1; 
		switch(curToken.subtype){
			case wordSize: allocationSz = 4; break;
			case halfSize: 
			allocationSz = halfSz;
			operandStack[operandIdx++] = (operand){halfSz, 4, constant, 0, 0, registerPermanence}; break;
			case byteSize: 
			allocationSz = byteSz;
			operandStack[operandIdx++] = (operand){byteSz, 4, constant, 0, 0, registerPermanence}; break;
		} break;
		case identifierToken:
		if(persistentTokens.foundAllocation){
			if(!(previousToken.type == sizeToken)) operandIdx--; persistentTokens.foundAllocation = 0;
			variableMetadata v = addVariable(curToken.str, curToken.len, allocationSz);
			if(persistentTokens.foundArgument){
				persistentTokens.foundArgument = 0;
			}
			operandStack[operandIdx++] = (operand){v.stackOffset, allocationSz, stackVar, 0, persistentTokens.foundFlush, registerPermanence};
		} else{
			const variableMetadata v = retrieveLocalVariable(curToken.str, curToken.len); if(v.name == (void*)0) return undefinedVariable;
			operandStack[operandIdx++] = (operand){v.stackOffset, v.size, stackVar, 0, persistentTokens.foundFlush, registerPermanence};
		} persistentTokens.foundFlush = 0;
		break;
		case opToken:
		persistentTokens.foundAllocation = 0;
		if(curToken.subtype == opSub && previousToken.type != constantToken && previousToken.type != identifierToken) curToken.subtype = opNegate;
		if(persistentTokens.foundVolatile && curToken.subtype == opDereference){curToken.subtype = opDereferenceVolatile; persistentTokens.foundVolatile = 0;}
		if(operatorIdx > 0){
			uint8_t combined = combineOperators(operatorStack[operatorIdx-1].subtype, curToken.subtype);
			if(combined != operatorStack[operatorIdx-1].subtype) {
				curToken.subtype = combined; operatorIdx--;
			}
		}const uint32_t curPrecedence = operatorPrecedence[curToken.subtype] + precedence;
		const operator curOp = (operator){curToken.subtype, curPrecedence};
		while(1){
			if(operatorIdx == 0) break;
			const uint16_t prevPrecedence = operatorStack[operatorIdx-1].precedence;
			if(curPrecedence > prevPrecedence) break;
			const operand tmp = assembleOp(operatorStack[--operatorIdx], operandStack + operandIdx - 1, registerPermanence);
			const uint8_t oIdx = operatorStack[operatorIdx].subtype; operandIdx -= numOperands(oIdx);
			operandStack[operandIdx++] = tmp;
		}operatorStack[operatorIdx++] = curOp; break;
		case endLine:
		persistentTokens.foundAllocation = 0;
		const uint8_t opErr = flushOperatorStacks(registerPermanence);
		if(opErr != noError) return opErr; operandIdx = 0;
		clearCompilerTemporaries();
		break;
		case keywordToken:
		persistentTokens.foundAllocation = 0;
		switch(curToken.subtype){
			case argumentKey:
			persistentTokens.foundArgument = 1;
			case flushKey:
			persistentTokens.foundFlush = 1; break;
			case ifKey: case whileKey:
			branchType[branchDepth] = curToken.subtype; break;
			case volatileKey: persistentTokens.foundVolatile = 1; break;
			case continueKey: emitOpcode(b_imm_32); emitArgument(0, 12);
			relativeLoopBlocks[relativeLoopIdx-1].continueAddrs[relativeLoopBlocks[relativeLoopIdx-1].numContinues] = progAddrC;
			relativeLoopBlocks[relativeLoopIdx-1].numContinues++; break;
			case breakKey: restoreSnapshot(relativeLoopBlocks[relativeLoopIdx-1].snapshot); 
			relativeLoopBlocks[relativeLoopIdx-1].breakAddrs[relativeLoopBlocks[relativeLoopIdx-1].numBreaks] = progAddrC;
			relativeLoopBlocks[relativeLoopIdx-1].numBreaks++; emitOpcode(b_imm_32); emitArgument(0, 12);
			break;
		} break;
		case clampToken:
		persistentTokens.foundAllocation = 0;
		switch(curToken.subtype){
			case parenthesesL: case parenthesesR:
			precedence += (curToken.subtype == parenthesesL) * 32 - (curToken.subtype == parenthesesR) * 32;
			break;
			case curlyBL:
			const uint32_t spa = progAddrC;
			const uint8_t cbi = addBlock();
			const uint8_t opErr = flushOperatorStacks(registerPermanence); clearCompilerTemporaries();
			if(opErr != noError) return opErr;
			operatorIdx = 0; operand condition; uint8_t invalidBackpatch = 0;
			switch(const uint8_t bt = branchType[branchDepth]){
				case elseKey: goto skipConditionalParsing;
				case whileKey: relativeLoopBlocks[relativeLoopIdx].jumpbackAddr = spa; relativeLoopBlocks[relativeLoopIdx].snapshot = getSnapshot();
				relativeLoopBlocks[relativeLoopIdx].backpatchAddr = progAddrC;
				case ifKey: condition = operandStack[--operandIdx];
				switch(condition.operandType){
					case constant:
					invalidBackpatch = 1;
					if(!condition.val.value){
						for(uint8_t depth = 1; depth;){
							nextToken();
							if(curToken.type == clampToken) depth += (curToken.subtype == curlyBL) - (curToken.subtype == curlyBR);
						}
					}break;
					case stackVar:
					operandStack[operandIdx++] = condition; operandStack[operandIdx] = (operand){0, 4, constant, 0, 0, registerPermanence};
					assembleOp((operator){opCmpNEqual, 0}, operandStack + operandIdx, registerPermanence); operandIdx -= 1;
				} if(bt == ifKey) {relativeIfBlocks[relativeIfIdx].jumpbackAddr = progAddrC;}
				emptyFlags(); clearCompilerTemporaries();
				break;
			} if(!invalidBackpatch){emitOpcode(bc_imm_32); emitFlag(getOppositeFlag(virtualFlags.flagType)); emitArgument(0, 12);}
			switch(branchType[branchDepth]){
				case whileKey: registerPermanence += 128;
				if(invalidBackpatch) relativeLoopBlocks[relativeLoopIdx].backpatchAddr = UINT32_MAX; break;
				case ifKey: if(invalidBackpatch) relativeIfBlocks[relativeIfIdx].jumpbackAddr = UINT32_MAX;
				relativeIfBlocks[relativeIfIdx].snapshot = getSnapshot();
				break;
			}
			registerPermanence += (branchType[branchDepth] == whileKey) * 128;
			clearCompilerTemporaries();
			skipConditionalParsing: incrementScopeKey(branchType[branchDepth], cbi); createChain(); 
			branchDepth++; break;
			case curlyBR:{
			branchDepth--;
			nextToken(); refreshToken = 0;
			decrementScopeKey(branchType[branchDepth]);
			for(uint8_t r = 0; r < maxGPRegs; r++) if(virtualRegFile[r].stackOffset >= curStackOffset) virtualRegFile[r].dirty = empty;
			uint8_t cbi; uint32_t backpatchAddr; switch(branchType[branchDepth]){
				case ifKey:
				backpatchAddr = relativeIfBlocks[relativeIfIdx].jumpbackAddr; cbi = relativeIfBlocks[relativeIfIdx].cbi;
				restoreSnapshot(relativeIfBlocks[relativeIfIdx].snapshot); 
				if(curToken.type == keywordToken && curToken.subtype == elseKey){branchType[branchDepth] = elseKey; 
					relativeIfBlocks[relativeIfIdx].jumpbackAddr = progAddrC; emitOpcode(b_imm_32); emitArgument(0, 12);
				}break;
				case elseKey:
				backpatchAddr = relativeIfBlocks[relativeIfIdx].jumpbackAddr; cbi = relativeIfBlocks[relativeIfIdx].cbi;
				restoreSnapshot(relativeIfBlocks[relativeIfIdx].snapshot); break;
				case whileKey:
				restoreSnapshot(relativeLoopBlocks[relativeLoopIdx].snapshot); 
				backpatchAddr = relativeLoopBlocks[relativeLoopIdx].backpatchAddr; cbi = relativeLoopBlocks[relativeLoopIdx].cbi;
				for(uint8_t c = 0; c < relativeLoopBlocks[relativeLoopIdx].numContinues; c++){
					printf("BACKPATCH CONTINUE\nLOCATION: %d  DATA: %d\n", relativeLoopBlocks[relativeLoopIdx].continueAddrs[c], progAddrC);
					backpatch_b_imm_32(relativeLoopBlocks[relativeLoopIdx].continueAddrs[c], progAddrC, outputBuf);
				}
				const int16_t bOff = relativeLoopBlocks[relativeLoopIdx].jumpbackAddr - progAddrC - 4;
				emitOpcode(b_imm_32); emitArgument(bOff/2, 16);
				for(uint8_t b = 0; b < relativeLoopBlocks[relativeLoopIdx].numBreaks; b++){
					printf("BACKPATCH BREAK\nLOCATION: %d  DATA: %d\n", relativeLoopBlocks[relativeLoopIdx].breakAddrs[b], progAddrC);
					backpatch_b_imm_32(relativeLoopBlocks[relativeLoopIdx].breakAddrs[b], progAddrC, outputBuf);
				}
				break;
			}	
			if(backpatchAddr != UINT32_MAX){
				backpatch(backpatchAddr, progAddrC, outputBuf);
			} endBlock(cbi);
		} endChain();
		break;
		}
		}
		if(curToken.type != nullToken) previousToken = curToken;
	}while(curToken.type != nullToken);
	if(precedence || curScope || (previousToken.type != endLine && !(previousToken.type == clampToken && previousToken.subtype == curlyBR))) return delimiterMismatch;
	flushInstructionBuffer(); printf("\nbinary output\n");
	for(uint32_t b = 0; b < progAddr; b++) printf("%x ", outputBuf[b]);
	return noError;
}