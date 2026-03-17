#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include "compiler.h"

#if defined(__GNUC__) || defined(__clang__)
	#define forceinline static inline __attribute__((always_inline))
#else
	#define forceinline static inline
#endif

#if defined(__x86_64__) || defined(_M_X64) || defined(__aarch64__) || defined(__LP64__)
    #define unionType uint64_t
    #define BITS 64
#else
#define unionType uint32_t
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
	opAdd, opSub, opIncrement, opDecrement, opMul, opScaleM, opDiv, opScaleD, 
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

const char ops[] = { '+', '-', '*', '%', '=', '/', '\\', '>', '<', '&', '|', 'T'};
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
	if(*src == '#'){do{src++;}while(*src != '#'); src++;}
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
	subs_imm_32, subs_reg_32, lsr_imm_32, lsl_imm_32, lsr_reg_32, lsl_reg_32,
	mulw_reg_32, divw_reg_32, ite_32, it_32,
	cmp_reg_32, cmp_imm_32, 
	eors_reg_32, eors_imm_32, orrs_reg_32, andw_imm_32, andw_reg_32, orrs_imm_32,
	mvn_imm_32, mvn_reg_32, b_imm_32, b_reg_32, bc_reg_32, bc_imm_32,
	ldrb_imm_16, strb_imm_16, ldrh_imm_16, strh_imm_16,
	b_imm_16, b_reg_16, bc_imm_16, bc_reg_16, mov_lit_16, ldr_imm_16, ldr_gpr_imm_16, str_imm_16, mov_reg_reg_16, add_reg_16
}opcode;

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
	uint8_t code; uint8_t numArgs;
	arg args[3];
}instruction;

#define makeInstruction(code) (instruction){.code = code, .numArgs = 0};
#define frameDepth 8
uint32_t progAddr; uint8_t* outputBuf; uint8_t modifierFrame;
instruction instructionFrame[frameDepth]; uint8_t numInstructions = 0;
instruction prevInstruction = (instruction){.code = 0xFF, .numArgs = 0xFF};

forceinline uint32_t setBits(uint32_t base, const uint32_t value, const uint8_t startBit){
	const uint32_t mask = value << startBit; return base | mask;
}	
#define args (instructionFrame[instructionIdx - 1].args)
forceinline void emitHex(const uint8_t instructionIdx) {
	const uint8_t code = instructionFrame[instructionIdx - 1].code;
	uint32_t emit = 0;
	uint8_t wrSz = 4;
	switch(code) {
	case movw_reg_reg_32:
	emit = setBits(emit, 0b1110101001001111, 16);
	emit = setBits(emit, args[1].val, 8);
	emit = setBits(emit, args[0].val, 0); 
	break;
	case movw_lit_32: 
	case movt_lit_32:
	emit = setBits(emit, 0b11110, 27);
	emit = setBits(emit, (args[1].val >> 11) & 1, 26);
	emit = setBits(emit, 0b100, 23);
	emit = setBits(emit, code == movt_lit_32, 22);
	emit = setBits(emit, 0b10, 20);
	emit = setBits(emit, (args[1].val >> 12) & 0b1111, 16);
	emit = setBits(emit, (args[1].val >> 8) & 0b111, 12);
	emit = setBits(emit, args[0].val, 8);
	emit = setBits(emit, args[1].val & 0b11111111, 0); 
	break;
	case ldrw_imm_32: case strw_imm_32: case ldrbw_imm_32: 
	case strbw_imm_32: case ldrhw_imm_32: case strhw_imm_32:
	emit = setBits(emit, 0b1111100, 25);
	emit = setBits(emit, (code == ldrhw_imm_32 || code == strhw_imm_32) ? 1 : (code == ldrw_imm_32 || code == strw_imm_32 ? 2 : 0), 23);
	emit = setBits(emit, (code == ldrw_imm_32 || code == ldrbw_imm_32 || code == ldrhw_imm_32), 22);
	emit = setBits(emit, 1, 20);
	emit = setBits(emit, args[0].val, 16);
	emit = setBits(emit, args[1].val, 12);
	emit = setBits(emit, args[2].val, 0); 
	break;
	case ldrw_reg_32: case strw_reg_32: case ldrbw_reg_32: 
	case strbw_reg_32: case ldrhw_reg_32: case strhw_reg_32:
	emit = setBits(emit, 0b1111100, 25);
	emit = setBits(emit, (code == ldrhw_reg_32 || code == strhw_reg_32) ? 1 : (code == ldrw_reg_32 || code == strw_reg_32 ? 2 : 0), 23);
	emit = setBits(emit, (code == ldrw_reg_32 || code == ldrbw_reg_32 || code == ldrhw_reg_32), 22);
	emit = setBits(emit, args[0].val, 16);
	emit = setBits(emit, args[1].val, 12);
	emit = setBits(emit, args[2].val, 0); 
	break;
	case addw_imm_32: case subw_imm_32: case subs_imm_32:
	emit = setBits(emit, 0b11110, 27);
	emit = setBits(emit, (args[2].val >> 11) & 1, 26);
	emit = setBits(emit, (code == addw_imm_32 ? 0b01000 : 0b01010) | (code == subs_imm_32), 20);
	emit = setBits(emit, args[0].val, 16);
	emit = setBits(emit, (args[2].val >> 8) & 0b111, 12);
	emit = setBits(emit, args[1].val, 8);
	emit = setBits(emit, args[2].val & 0xFF, 0); 
	break;
	case addw_reg_32: case subw_reg_32: case subs_reg_32:
	emit = setBits(emit, 0b1110101, 25);
	emit = setBits(emit, (code == addw_reg_32 ? (modifierFrame ? 0b1010 : 0b1000) : (modifierFrame ? 0b1011 : 0b1101)) | (code == subs_reg_32), 20);
	emit = setBits(emit, args[0].val, 16);
	emit = setBits(emit, args[1].val, 8);
	emit = setBits(emit, args[2].val, 0); 
	break;
	case lsr_imm_32: 
	case lsl_imm_32:
	emit = setBits(emit, 0b1110101001001111, 16);
	emit = setBits(emit, (args[2].val >> 2) & 0b111, 12);
	emit = setBits(emit, args[1].val, 8);
	emit = setBits(emit, (args[2].val & 0b11) << 6 | (code == lsr_imm_32 ? 0b100000 : 0), 0);
	emit = setBits(emit, args[0].val, 0); 
	break;
	case mulw_reg_32: 
	case divw_reg_32:
	emit = setBits(emit, 0b11111011, 24);
	emit = setBits(emit, code == divw_reg_32 ? 0b1001 : 0, 20);
	emit = setBits(emit, args[0].val, 16);
	emit = setBits(emit, 0b1111, 12);
	emit = setBits(emit, args[1].val, 8);
	emit = setBits(emit, args[2].val, 0); 
	break;
	case ite_32: 
	case it_32: 
	wrSz = 2; 
	emit = setBits(emit, 0xBF00 | (args[0].val << 4) | (code == ite_32 ? 0b0110 : 0b1000), 0); 
	break;
	case cmp_reg_32: 
	case cmp_imm_32:
	emit = setBits(emit, code == cmp_imm_32 ? 0xF1B00F00 : 0xEBB00F00, 0);
	emit = setBits(emit, args[0].val, 16);
	emit = setBits(emit, args[1].val, 0); 
	break;
	case andw_reg_32: case andw_imm_32: 
	case orrs_reg_32: case orrs_imm_32: 
	case eors_reg_32: case eors_imm_32:
	emit = setBits(emit, (code == andw_reg_32 || code == andw_imm_32) ? 0xEA000000 : (code == orrs_reg_32 || code == orrs_imm_32 ? 0xEA400000 : 0xEA800000), 0);
	emit = setBits(emit, args[0].val, 16);
	emit = setBits(emit, args[1].val, 8);
	emit = setBits(emit, args[2].val, 0); 
	break;
	case mvn_imm_32: 
	case mvn_reg_32:
	emit = setBits(emit, code == mvn_imm_32 ? 0xF06F0000 : 0xEA6F0000, 0);
	emit = setBits(emit, args[0].val, 8);
	emit = setBits(emit, args[1].val, 0); 
	break;
	case b_imm_32: 
	case bc_imm_32:
	emit = setBits(emit, (code == bc_imm_32 ? 0xF0008000 : 0xF0009000), 0);
	emit = setBits(emit, args[0].val, 0); 
	break;
	case ldrb_imm_16: case strb_imm_16: case ldrh_imm_16: 
	case strh_imm_16: case ldr_imm_16: case str_imm_16: wrSz = 2;
	emit = setBits(emit, (code == ldrb_imm_16 ? 0x7800 : code == strb_imm_16 ? 0x7000 : code == ldrh_imm_16 ? 0x8800 : code == strh_imm_16 ? 0x8000 : code == ldr_imm_16 ? 0x6800 : 0x6000), 0);
	emit = setBits(emit, args[2].val << 6 | args[0].val << 3 | args[1].val, 0); 
	break;
	case b_imm_16: 
	case bc_imm_16: wrSz = 2;
	emit = setBits(emit, code == b_imm_16 ? 0xE000 : 0xD000, 0);
	emit = setBits(emit, args[0].val, 0); 
	break;
	case mov_lit_16: wrSz = 2;
	emit = setBits(emit, 0x2000 | (args[0].val << 8) | (args[1].val & 0xFF), 0); 
	break;
	case mov_reg_reg_16: wrSz = 2;
	emit = setBits(emit, 0x4600 | (args[1].val << 3) | args[0].val, 0); 
	break;
	case add_reg_16: wrSz = 2;
	emit = setBits(emit, (modifierFrame ? 0x4140 : 0x4400) | (args[1].val << 3) | args[0].val, 0); 
	break;
	} for(uint8_t b = 0; b < wrSz; b++) outputBuf[progAddr++] = (emit >> (b * 8)) & 0xFF;
}
#undef args

forceinline void shiftInstructionFrame(const uint8_t rmIdx){
	for(uint8_t i1 = rmIdx, i2 = rmIdx + 1; i2 < frameDepth; i2++, i1++) instructionFrame[i1] = instructionFrame[i2];
}
#define isLDR(c) (c == ldr_gpr_imm_16 || c == ldr_imm_16 || c == ldrb_imm_16 || c == ldrbw_imm_32 || c == ldrbw_reg_32 || c == ldrh_imm_16 || c == ldrhw_imm_32 || c == ldrhw_reg_32 || c == ldrw_imm_32 || c == ldrw_reg_32)
#define isSTR(c) (c == str_imm_16 || c == strb_imm_16 || c == strbw_imm_32 || c == strbw_reg_32 || c == strh_imm_16 || c == strhw_imm_32 || c == strhw_reg_32 || c == strw_imm_32 || c == strw_reg_32)
forceinline uint8_t isPipelineBubble(const instruction i1, const instruction i2){
	switch(i1.code){
		case ldr_gpr_imm_16: case ldr_imm_16: case ldrb_imm_16: case ldrbw_imm_32: case ldrbw_reg_32:
		case ldrh_imm_16: case ldrhw_imm_32: case ldrhw_reg_32: case ldrw_imm_32: case ldrw_reg_32:
		for(uint8_t i = 0; i < i2.numArgs; i++)if(i2.args[i].type == registerArg && i2.args[i].val == i1.args[0].val) return 1;
		return isLDR(i2.code);
	}
}
enum orderTypes{noOrder, requiredOrder, redundantOrder};
forceinline uint8_t orderRequirements(const instruction i1, const instruction i2){
	// check if i1 writes into registers that are read in i2 OR if i1 reads registers that i2 later writes to.
	// if so, order is required
	// check if i1 and i2 store into same register. if so, order is redundant
	// else, its just no order
}

void emitOpcode(const uint8_t code) {
	switch (code) {
	case movw_reg_reg_32: printf("MOVW_REG_REG_32"); break;
	case movw_lit_32:    printf("MOVW_LIT_32"); break;
	case movt_lit_32:    printf("MOVT_LIT_32"); break;
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
	case add_reg_16:     printf("ADD_REG_16"); break;
	case mulw_reg_32:    printf("MULW_REG_32"); break;
	case divw_reg_32:    printf("DIVW_REG_32"); break;
	case eors_imm_32:	 printf("EORS_IMM_32"); break;
	case eors_reg_32:	 printf("EORS_REG_32"); break;
	case orrs_reg_32:	 printf("ORRS_REG_32"); break;
	case orrs_imm_32:	 printf("ORRS_IMM_32"); break;
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
	}if(numInstructions == frameDepth){
		prevInstruction = instructionFrame[0];
		emitHex(1);
		shiftInstructionFrame(0);
	}
	instructionFrame[numInstructions++] = makeInstruction(code);
	printf("\n");
	fflush(stdout);
}
forceinline void flushInstructionBuffer(){
	for(uint8_t i = 1; i <= numInstructions; i++) emitHex(i);
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

#define maxGPRegs 12
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
		emitOpcode(movw_lit_32); emitArgument(reg, 4); emitArgument(c, 16);
		if(c > 65535){emitOpcode(movt_lit_32); emitArgument(reg, 4); emitArgument(c, 16);}
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
	uint32_t stackOffset, size;
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
uint32_t curStackOffset = 0; uint32_t curCompilerTempSz = 0;
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

forceinline uint32_t evalImm(const uint32_t o1, const uint32_t o2, const uint8_t subtype){
	switch(subtype){
		case opAdd: return o1 + o2;
		case opSub: return o1 - o2;
		case opMul: return o1 * o2;
		case opDiv: return o1 / o2;
		case opEqual: return o1 == o2;
		case opBitwiseAnd: return o1 & o2;
		case opBitwiseOr: return o1 | o2;
		case opBitwiseXor: return o1 ^ o2;
		case opCmpEqual: return o1 == o2;
	}
}

forceinline uint8_t returnImmOpcode(const uint8_t subtype){
	switch(subtype){
		case opAdd: case opIncrement: return addw_imm_32;
		case opSub: case opDecrement: return subw_imm_32;
		case opMul: return mulw_reg_32;
		case opBitwiseAnd: return andw_imm_32;
		case opBitwiseOr: return orrs_imm_32;
		case opBitwiseNot: return mvn_imm_32;
		case opRightShift: return lsr_imm_32;
		case opLeftShift: return lsl_imm_32;
	}
} forceinline uint8_t returnRegOpcode(const uint8_t subtype){
	switch(subtype){
		case opAdd: case opIncrement: return addw_reg_32;
		case opSub: case opDecrement: return subw_reg_32;
		case opMul: return mulw_reg_32;
		case opBitwiseAnd: return andw_reg_32;
		case opBitwiseOr: return orrs_reg_32;
		case opBitwiseNot: return mvn_reg_32;
		case opRightShift: return lsr_reg_32;
		case opLeftShift: return lsl_reg_32;
	}
}

forceinline uint8_t immSize(const uint8_t subtype){
	switch(subtype){
		case opBitwiseAnd: case opBitwiseXor: case opBitwiseOr: return 8;
		case opIncrement: case opDecrement: case opAdd: case opSub: case opBitwiseNot: return 16;
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
		case flag_gt: return flag_le;
		case flag_lt: return flag_ge;
		case flag_ge: return flag_lt;
		case flag_le: return flag_gt;
	}
}

forceinline uint8_t numOperands(const uint8_t subtype){
	switch(subtype){
		case opWritebackArr: return 4;
		case opWriteback: case opDereferenceArr: return 3;
		case opFree: case opLogicalNot: case opBitwiseNot: case opReference: return 1;
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
			if(wIdx >= num32BitTransfers) {const uint8_t r = moveConstantToRegs(0, o1.val.stackOffset + wIdx * 4, registerPermanence); virtualRegFile[r].dirty = dirty; goto skipWriteback;}
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
				if((o2.val.stackOffset + wIdx * 4) < (curStackOffset - curCompilerTempSz) || (o2.val.stackOffset + wIdx * 4) >= curStackOffset){
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
		case opDereferenceVolatile: case opDereference:{
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
					const uint8_t r = moveOffsetToRegs(o2.val.value + wIdx * 4, o2.val.value + wIdx * 4, UINT16_MAX);
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
		case opWriteback:{
		int8_t addrReg = -1;  uint32_t addrConst; uint8_t addrd;
		const operand o3 = *operands; o2 = *(operands - 1); o1 = *(operands - 2);
		const uint8_t sizeType = o1.val.value == halfSz ? halfSize : (o1.val.value == byteSz ? byteSize : wordSize);
		const uint32_t num32BitTransfers = o1.val.value >= (UINT32_MAX - 1) ? 1 : o1.val.value;
		switch(o2.operandType){
			case constant:
			for(uint8_t r = 0; r < maxGPRegs; r++) if(virtualRegFile[r].stackOffset >= o2.val.stackOffset + o3.val.value && virtualRegFile[r].stackOffset < o2.val.stackOffset + o3.val.value + num32BitTransfers* 4) 
			virtualRegFile[r].dirty = empty; addrConst = o2.val.value; break;
			case stackVar:
			movedFromStack = 0;
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
			flushRegisters(); 
		}const uint32_t offsetCheck = curStackOffset + (curCompilerTempSz == 0);
		for(uint32_t wIdx = 0; wIdx < num32BitTransfers; wIdx++){
			uint8_t readReg;
			if(wIdx >= (o3.size + 3) >> 2){readReg = moveConstantToRegs(0, UINT32_MAX, UINT16_MAX); virtualRegFile[readReg].dirty = empty;}
			else switch(o3.operandType){
				case constant:
				readReg = moveConstantToRegs(o3.val.value, UINT32_MAX, UINT16_MAX); virtualRegFile[readReg].dirty = empty;
				break;
				case stackVar:
				readReg = moveOffsetToRegs(o3.val.stackOffset + wIdx * 4, o3.val.stackOffset + wIdx * 4, o3.registerPreference);
				break;
			}
			if(addrReg > -1){
				if(num32BitTransfers-1){
					storeRegIntoStackRegPointer(readReg, addrReg, wIdx * 4, sizeType);
				}else{
					storeRegisterIntoStackR(readReg, addrReg, sizeType);
				}
			}
			else storeRegisterIntoStack(readReg, addrConst + wIdx * 4, sizeType);
			if(virtualRegFile[readReg].stackOffset >= curStackOffset - curCompilerTempSz && virtualRegFile[readReg].stackOffset < offsetCheck) virtualRegFile[readReg].dirty = empty;
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
			virtualRegFile[r].dirty = empty; break;
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
		case opSub: case opAdd: case opBitwiseAnd: case opBitwiseOr: case opRightShift: case opLeftShift:{
		o2 = *operands, o1 = *(operands - 1);
		const uint32_t num32BitAdds = ((o1.size > o2.size ? o1.size : o2.size) + 3) >> 2;
		for(uint32_t wIdx = 0; wIdx < num32BitAdds; wIdx++){
			int64_t constantInAdd = -1; int8_t r1 = -1; uint8_t r1d, r2d;
			switch(o1.operandType){
				case constant:
				constantInAdd = wIdx ? 0 : o1.val.value;
				if(o1.val.value >= 2 << immSize(op.subtype) && o2.operandType != constant && !wIdx) r1 = moveConstantToRegs(o1.val.value, curStackOffset - curCompilerTempSz, UINT16_MAX);
				break;
				case stackVar:
				r1 = moveOffsetToRegs(o1.val.stackOffset + wIdx * 4, o1.val.stackOffset + wIdx * 4, o1.registerPreference);
				r1d = virtualRegFile[r1].dirty;
				break;
			} if(r1 != -1) virtualRegFile[r1].dirty = locked;
			int8_t r2 = -1;
			switch(o2.operandType){
				case constant:
				if(constantInAdd >= 0) {return (operand) {evalImm(constantInAdd, o2.val.value, op.subtype), 4, constant, 0, 0, registerPermanence};}
				if(!wIdx){constantInAdd = o2.val.value; if(o2.val.value >= 2 << immSize(op.subtype)) r2 = moveConstantToRegs(o2.val.value, curStackOffset - curCompilerTempSz, UINT16_MAX);}
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
			}const uint32_t compilerSz = curStackOffset + (curCompilerTempSz == 0);
			if(virtualRegFile[r1].stackOffset >= curStackOffset - curCompilerTempSz && virtualRegFile[r1].stackOffset < compilerSz)
			virtualRegFile[r1].dirty = empty; else virtualRegFile[r1].dirty = r1d;
			if(r2 != -1 && virtualRegFile[r2].stackOffset >= curStackOffset - curCompilerTempSz && virtualRegFile[r2].stackOffset < compilerSz)
			virtualRegFile[r2].dirty = empty; else virtualRegFile[r2].dirty = r2d;
			const uint8_t resultReg = getEmptyRegister(curStackOffset + wIdx * 4, registerPermanence, noCheck); virtualRegFile[resultReg].dirty = dirty;
			if(r2 == -1) emitOpcode(returnImmOpcode(op.subtype)); else emitOpcode(returnRegOpcode(op.subtype));
			if(op.subtype == opAdd || op.subtype == opSub) emitModifier(wIdx == 0);
			emitArgument(resultReg, 4); emitArgument(r1, 4); emitArgument(r2 == -1 ? constantInAdd : r2, r2 == -1 ? immSize(op.subtype) : 4);
		}
		curStackOffset += num32BitAdds * 4; curCompilerTempSz += num32BitAdds * 4;
		return (operand){curStackOffset - num32BitAdds * 4, num32BitAdds * 4, stackVar, 0, 0, registerPermanence};
		}
		case opIncrement: case opDecrement:{
		o2 = *operands, o1 = *(operands - 1);
		const uint32_t num32BitAdds = ((o1.size > o2.size ? o1.size : o2.size) + 3) >> 2;
		for(uint32_t wIdx = 0; wIdx < num32BitAdds; wIdx++){
			int64_t constantInAdd = -1; int8_t r1 = -1; uint8_t r1d, r2d;
			r1 = moveOffsetToRegs(o1.val.stackOffset + wIdx * 4, o1.val.stackOffset + wIdx * 4, registerPermanence);
			r1d = virtualRegFile[r1].dirty; virtualRegFile[r1].dirty = locked; int8_t r2 = -1;
			switch(o2.operandType){
				case constant:
				if(!wIdx){constantInAdd = o2.val.value; if(o2.val.value >= 2 << immSize(op.subtype)) r2 = moveConstantToRegs(o2.val.value, curStackOffset - curCompilerTempSz, UINT16_MAX);}
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
			if(r2 == -1) emitOpcode(returnImmOpcode(op.subtype)); else emitOpcode(returnRegOpcode(op.subtype));
			emitModifier(wIdx == 0);
			emitArgument(r1, 4); emitArgument(r1, 4); emitArgument(r2 == -1 ? constantInAdd : r2, r2 == -1 ? immSize(op.subtype) : 4);
		}
		curStackOffset += num32BitAdds * 4; curCompilerTempSz += num32BitAdds * 4;
		return (operand){curStackOffset - num32BitAdds * 4, num32BitAdds * 4, stackVar, 0, 0, registerPermanence};
			
		}
		case opCmpEqual: case opCmpGreater: case opCmpLess: case opCmpGEqual: case opCmpLEqual: case opCmpNEqual:{
		o2 = *operands, o1 = *(operands - 1);
		flushFlags();
		virtualFlags.flagType = op.subtype; virtualFlags.dirty = dirty;
		if(o1.operandType == constant && o2.operandType == constant) return (operand) {4, 4, constant, 0, 0, registerPermanence};
		const uint32_t num32BitCmps = ((o1.size > o2.size ? o1.size : o2.size) + 3) >> 2;
		for(uint32_t wIdx = 0; wIdx < num32BitCmps; wIdx++){
			int8_t r1 = -1; uint32_t constantInCmp = 0; uint8_t r1d, r2d;
			switch(o1.operandType){
				case constant:
				if(o1.val.value >= UINT8_MAX) r1 = moveConstantToRegs(o1.val.value, curStackOffset - curCompilerTempSz, UINT16_MAX);
				else constantInCmp = o1.val.value; break;
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
		case opMul: case opDiv:{
		o2 = *operands, o1 = *(operands - 1);
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
    4, 4, 1, 1, 5, 1, 5, 1, // Add, Sub, Inc, Dec, Mul, scaleMul, Div, scaleDiv
    5, 5, 1, 1,          // Right Shift, Left Shift, Equal (Assignment), Free (Manual register management shi)
    8, 8, 8, 8, 8,    // Reference(Unary), Writeback, Dereference, Array Writeback, Array Dereference
    4, 4, 8, 4, // Bitwise: Or, And, Not(Unary), Xor
    2, 2, 8,    // Logical: And, Or, Not(Unary)
    3, 3, 3, 3, 3, 3, // Comparisons: Greater, Less, Equal, GEqual, NEqual, LEqual
    7, 7        // Volatile Deref/Writeback
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
	const uint32_t trueJump = progAddr - addressToPatch - 4;
	progOrigin[addressToPatch] = trueJump;
}

#define branchKeywordLimit 16
typedef struct{
	uint32_t jumpbackAddr;
	registerSnapshot snapshot;
	uint32_t breakAddrs[branchKeywordLimit]; uint8_t numBreaks;
	uint32_t continueAddrs[branchKeywordLimit]; uint8_t numContinues;
}relativeLoopOffset;
#define emptyLoopOffset (relativeLoopOffset){.numBreaks = 0}
typedef struct{
	uint32_t jumpbackAddr; registerSnapshot snapshot;
}relativeIfOffset;
relativeLoopOffset relativeLoopBlocks[maxBranchDepth]; uint8_t relativeLoopIdx;
relativeIfOffset relativeIfBlocks[maxBranchDepth]; uint8_t relativeIfIdx;

forceinline uint8_t combineOperators(uint8_t opIdx){
	if(opIdx == 0) return 0;
	switch(operatorStack[opIdx].subtype){
		case opEqual:
		switch(operatorStack[opIdx-1].subtype){
			case opDereference: operatorStack[--opIdx].subtype = opWriteback; break;
			case opDereferenceArr: operatorStack[--opIdx].subtype = opWritebackArr; break;
			case opAdd: operatorStack[--opIdx].subtype = opIncrement; break;
			case opSub: operatorStack[--opIdx].subtype = opDecrement; break;
			case opMul: operatorStack[--opIdx].subtype = opScaleM; break;
			case opDiv: operatorStack[--opIdx].subtype = opScaleD; break;
		}
		break;
		case opCmpEqual:
		switch(operatorStack[opIdx-1].subtype){
			case opLogicalNot: operatorStack[--opIdx].subtype = opCmpNEqual; break;
			case opCmpGreater: operatorStack[--opIdx].subtype = opCmpGEqual; break;
			case opCmpLess: operatorStack[--opIdx].subtype = opCmpLEqual; break;
		} break;
	}
	return opIdx;
}

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
forceinline void incrementScopeKey(const uint8_t keyType){
	incrementScope(); switch(keyType){
	case ifKey: case elseKey: relativeIfIdx++; break;
	case whileKey: relativeLoopIdx++;
	}
}

#pragma pack(push, 1)
typedef struct{
	uint8_t foundAllocation : 1;
	uint8_t foundFlush : 1;
	uint8_t foundVolatile : 1;
	uint8_t foundNegation : 1;
}persistentToken;
#pragma pack(pop, 1)

uint8_t branchType[maxBranchDepth]; uint8_t branchDepth = 0;
uint8_t operatorIdx, operandIdx;

forceinline uint8_t flushOperatorStacks(const uint32_t registerPermanence){
	for(int8_t idx = operatorIdx - 1; idx >= 0; idx--){
		idx = combineOperators(idx);
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
	initializeVirtualFlags(); initializeVirtualRegs(); curScope = 0; progAddr = 0;
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
		} if(persistentTokens.foundNegation){literal = ~literal + 1; persistentTokens.foundNegation = 0;}
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
			const variableMetadata v = addVariable(curToken.str, curToken.len, allocationSz);
			operandStack[operandIdx++] = (operand){v.stackOffset, allocationSz, stackVar, 0, persistentTokens.foundFlush, registerPermanence};
		} else{
			const variableMetadata v = retrieveLocalVariable(curToken.str, curToken.len); if(v.name == (void*)0) return undefinedVariable;
			operandStack[operandIdx++] = (operand){v.stackOffset, v.size, stackVar, 0, persistentTokens.foundFlush, registerPermanence};
		} persistentTokens.foundFlush = 0;
		break;
		case opToken:
		persistentTokens.foundAllocation = 0;
		const uint32_t curPrecedence = operatorPrecedence[curToken.subtype] + precedence;
		if(previousToken.type == opToken) goto skipWhileParse;
		if(persistentTokens.foundVolatile && curToken.subtype == opDereference) curToken.subtype = opDereferenceVolatile;
		while(1){
			uint32_t prevPrecedence = operatorIdx > 0 ? operatorStack[operatorIdx-1].precedence : 0;
			if(curPrecedence > prevPrecedence || operatorIdx == 1) break;
			const operand tmp = assembleOp(operatorStack[--operatorIdx], operandStack + operandIdx - 1, registerPermanence);
			const uint8_t oIdx = operatorStack[operatorIdx].subtype; operandIdx -= numOperands(oIdx);
			operandStack[operandIdx++] = tmp;
		} skipWhileParse:; operatorStack[operatorIdx++] = (operator){curToken.subtype, curPrecedence};
		break;
		case endLine:
		persistentTokens.foundAllocation = 0;
		const uint8_t opErr = flushOperatorStacks(registerPermanence);
		if(opErr != noError) return opErr; operandIdx = 0;
		clearCompilerTemporaries();
		break;
		case keywordToken:
		persistentTokens.foundAllocation = 0;
		switch(curToken.subtype){
			case flushKey:
			persistentTokens.foundFlush = 1; break;
			case ifKey: case whileKey:
			branchType[branchDepth] = curToken.subtype; break;
			case volatileKey: persistentTokens.foundVolatile = 1; break;
			case continueKey: emitOpcode(b_imm_32); emitArgument(4096, 12);
			relativeLoopBlocks[relativeLoopIdx-1].continueAddrs[relativeLoopBlocks[relativeLoopIdx-1].numContinues] = progAddr;
			relativeLoopBlocks[relativeLoopIdx-1].numContinues++; break;
			case breakKey: restoreSnapshot(relativeLoopBlocks[relativeLoopIdx-1].snapshot); 
			relativeLoopBlocks[relativeLoopIdx-1].breakAddrs[relativeLoopBlocks[relativeLoopIdx-1].numBreaks] = progAddr;
			relativeLoopBlocks[relativeLoopIdx-1].numBreaks++; emitOpcode(b_imm_32); emitArgument(4096, 12);
			break;
		} break;
		case clampToken:
		persistentTokens.foundAllocation = 0;
		switch(curToken.subtype){
			case parenthesesL: case parenthesesR:
			precedence += (curToken.subtype == parenthesesL) * 16 - (curToken.subtype == parenthesesR) * 16;
			break;
			case curlyBL:
			const uint8_t opErr = flushOperatorStacks(registerPermanence);
			if(opErr != noError) return opErr;
			operatorIdx = 0; operand condition; uint8_t invalidBackpatch = 0;
			switch(branchType[branchDepth]){
				case elseKey: goto skipConditionalParsing;
				case whileKey: relativeLoopBlocks[relativeLoopIdx].jumpbackAddr = progAddr; relativeLoopBlocks[relativeLoopIdx].snapshot = getSnapshot();
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
					assembleOp((operator){opCmpEqual, 0}, operandStack + operandIdx, registerPermanence); operandIdx -= 2;
				} if(branchType[branchDepth] == ifKey) relativeIfBlocks[relativeIfIdx].jumpbackAddr = progAddr;
				emptyFlags(); clearCompilerTemporaries();
				break;
			} if(!invalidBackpatch){emitOpcode(bc_imm_32); emitFlag(getOppositeFlag(virtualFlags.flagType)); emitArgument(4096, 12);}
			switch(branchType[branchDepth]){
				case whileKey: registerPermanence += 128;
				if(invalidBackpatch) relativeLoopBlocks[relativeLoopIdx].jumpbackAddr = UINT32_MAX; break;
				case ifKey: if(invalidBackpatch) relativeIfBlocks[relativeIfIdx].jumpbackAddr = UINT32_MAX;
				relativeIfBlocks[relativeIfIdx].snapshot = getSnapshot();
				break;
			}
			registerPermanence += (branchType[branchDepth] == whileKey) * 128;
			clearCompilerTemporaries();
			skipConditionalParsing: incrementScopeKey(branchType[branchDepth]); 
			branchDepth++; break;
			case curlyBR:
			branchDepth--;
			nextToken(); refreshToken = 0;
			decrementScopeKey(branchType[branchDepth]);
			for(uint8_t r = 0; r < maxGPRegs; r++) if(virtualRegFile[r].stackOffset >= curStackOffset) virtualRegFile[r].dirty = empty;
			uint32_t backpatchAddr; switch(branchType[branchDepth]){
				case ifKey:
				backpatchAddr = relativeIfBlocks[relativeIfIdx].jumpbackAddr; 
				restoreSnapshot(relativeIfBlocks[relativeIfIdx].snapshot); 
				if(curToken.type == keywordToken && curToken.subtype == elseKey){branchType[branchDepth] = elseKey; 
					relativeIfBlocks[relativeIfIdx].jumpbackAddr = progAddr; emitOpcode(b_imm_32); emitArgument(4096, 12);
				}break;
				case elseKey:
				backpatchAddr = relativeIfBlocks[relativeIfIdx].jumpbackAddr;
				restoreSnapshot(relativeIfBlocks[relativeIfIdx].snapshot); break;
				case whileKey:
				backpatchAddr = relativeLoopBlocks[relativeLoopIdx].jumpbackAddr + 4;
				for(uint8_t c = 0; c < relativeLoopBlocks[relativeLoopIdx].numContinues; c++){
					printf("BACKPATCH CONTINUE\nLOCATION: %d  DATA: %d\n", relativeLoopBlocks[relativeLoopIdx].continueAddrs[c], backpatchAddr);
				}restoreSnapshot(relativeLoopBlocks[relativeLoopIdx].snapshot); break;
			}
			if(branchType[branchDepth] == whileKey){
				emitOpcode(b_imm_32); emitArgument((backpatchAddr - 4) - progAddr - 4, 16);
				for(uint8_t b = 0; b < relativeLoopBlocks[relativeLoopIdx].numBreaks; b++){
					printf("BACKPATCH BREAK\nLOCATION: %d  DATA: %d\n", relativeLoopBlocks[relativeLoopIdx].breakAddrs[b], progAddr);
				}
			}if(backpatchAddr - 4 != UINT32_MAX){
				printf("BACKPATCH %d\n", progAddr - backpatchAddr - 4); 
				backpatch(backpatchAddr, progAddr, progOrigin);
			}
		}
		break;
		}
		if(curToken.type != nullToken) previousToken = curToken;
	}while(curToken.type != nullToken);
	if(precedence || curScope || (previousToken.type != endLine && !(previousToken.type == clampToken && previousToken.subtype == curlyBR))) return delimiterMismatch;
	flushInstructionBuffer(); printf("\nbinary output\n");
	for(uint32_t b = 0; b < progAddr; b++) printf("%x ", outputBuf[b]);
	return noError;
}