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
    // 32-bit code here
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
	opAdd, opSub, opMul, opDiv, opEqual, opReference, opWriteback,
	opDereference, opBitwiseOr, opBitwiseAnd, opBitwiseNot, opBitwiseXor, opLogicalAnd,
	opLogicalOr, opLogicalNot, opCmpGreater, opCmpLess, opCmpEqual, opCmpGEqual, opCmpNEqual, opCmpLEqual,
	opDereferenceVolatile, opWritebackVolatile
};

enum clampSubtype {
	parenthesesL, parenthesesR,
	curlyBL, curlyBR
};

enum keywordSubtype {
	ifKey, whileKey,
	breakKey, continueKey, flushKey, volatileKey
};

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
	for(uint8_t tIdx = 0; *lit != '\0'; tIdx++, lit++){
		if(tIdx == s.len) return 0;
		if(s.str[tIdx] != *lit) return 0;
	} return 1;
}

char* src; token curToken;
forceinline void setSource(const char* c){src = c;}

void nextToken(){
	while(isNullChar(*src))src++;
	if(*src == '#') do{src++;}while(*src != '#');
	curToken = (token){(void*)0, 0, nullToken, 0}; if(*src == '\0')return;
	curToken.str = src; curToken.len = 0;
	if(isSingle(*src)){src++; curToken.len++;}
	else{while(!isNullChar(*src) && !isSingle(*src)){src++; curToken.len++;}}
	switch(*(curToken.str)){
		case '+': curToken.type = opToken; curToken.subtype = opAdd;break;
		case '-': curToken.type = opToken; curToken.subtype = opSub;break;
		case '*': curToken.type = opToken; curToken.subtype = opMul;break;
		case '/': curToken.type = opToken; curToken.subtype = opDiv;break;
		case '=': curToken.type = opToken; curToken.subtype = opEqual;break;
		case '>': curToken.type = opToken; curToken.subtype = opCmpGreater;break;
		case '<': curToken.type = opToken; curToken.subtype = opCmpLess;break;
		case '|': curToken.type = opToken; curToken.subtype = opBitwiseOr;break;
		case '&': curToken.type = opToken; curToken.subtype = opBitwiseAnd;break;
		case '!': curToken.type = opToken; curToken.subtype = opBitwiseNot;break;
		case '(': curToken.type = clampToken; curToken.subtype = parenthesesL;break;
		case ')': curToken.type = clampToken; curToken.subtype = parenthesesR;break;
		case '{': curToken.type = clampToken; curToken.subtype = curlyBL;break;
		case '}': curToken.type = clampToken; curToken.subtype = curlyBR;break;
		case ';': curToken.type = endLine; curToken.subtype = 0;break;
		case 'r': if(tokenCmpLiteral(curToken, "ref")){curToken.type = opToken; curToken.subtype = opReference;}
		else{curToken.type = identifierToken; curToken.subtype = 0;} break;
		case 'd': if(tokenCmpLiteral(curToken, "deref")){curToken.type = opToken; curToken.subtype = opDereference;}
		else{curToken.type = identifierToken; curToken.subtype = 0;} break;
		case 'o': if(tokenCmpLiteral(curToken, "or")){curToken.type = opToken; curToken.subtype = opLogicalOr;}
		else{curToken.type = identifierToken; curToken.subtype = 0;} break;
		case 'a': if(tokenCmpLiteral(curToken, "and")){curToken.type = opToken; curToken.subtype = opLogicalAnd;}
		else{curToken.type = identifierToken; curToken.subtype = 0;} break;
		case 'n': if(tokenCmpLiteral(curToken, "not")){curToken.type = opToken; curToken.subtype = opLogicalNot;}
		else{curToken.type = identifierToken; curToken.subtype = 0;} break;
		case 'e': if(tokenCmpLiteral(curToken, "equals")){curToken.type = opToken; curToken.subtype = opCmpEqual;}
		else{curToken.type = identifierToken; curToken.subtype = 0;} break;
		case 'g': if(tokenCmpLiteral(curToken, "greater")){curToken.type = opToken; curToken.subtype = opCmpGreater;}
		else{curToken.type = identifierToken; curToken.subtype = 0;} break;
		case 'l': if(tokenCmpLiteral(curToken, "less")){curToken.type = opToken; curToken.subtype = opCmpLess;}
		else{curToken.type = identifierToken; curToken.subtype = 0;} break;
		case 'i': if(tokenCmpLiteral(curToken, "if")){curToken.type = keywordToken; curToken.subtype = ifKey;}
		else{curToken.type = identifierToken; curToken.subtype = 0;} break;
		case 'c': if(tokenCmpLiteral(curToken, "continue")){curToken.type = keywordToken; curToken.subtype = continueKey;}
		else{curToken.type = identifierToken; curToken.subtype = 0;} break;
		case 'b': if(tokenCmpLiteral(curToken, "break")){curToken.type = keywordToken; curToken.subtype = breakKey;}
		else{curToken.type = identifierToken; curToken.subtype = 0;} break;
		case 'f': if(tokenCmpLiteral(curToken, "flush")){curToken.type = keywordToken; curToken.subtype = flushKey;}
		else{curToken.type = identifierToken; curToken.subtype = 0;} break;
		case 'v': if(tokenCmpLiteral(curToken, "volatile")){curToken.type = keywordToken; curToken.subtype = volatileKey;}
		else{curToken.type = identifierToken; curToken.subtype = 0;} break;
		case 'w': if(tokenCmpLiteral(curToken, "while")){curToken.type = keywordToken; curToken.subtype = whileKey;}
		else if(tokenCmpLiteral(curToken, "word")){curToken.type = sizeToken; curToken.subtype = 0;}
		else{curToken.type = identifierToken; curToken.subtype = 0;} break;
		default:
		if(*(curToken.str) >= '0' && *(curToken.str) < '9'){curToken.type = constantToken; curToken.subtype = 0;}
		else{curToken.type = identifierToken; curToken.subtype = 0;}
	}	
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
	subw_imm_32, addw_imm_32, subw_reg_32, addw_reg_32,
	subs_imm_32, subs_reg_32,
	mulw_reg_32, divw_reg_32, ite_32, it_32,
	cmp_reg_32, cmp_imm_32, 
	eors_reg_32, eors_imm_32, orrs_reg_32, andw_imm_32, andw_reg_32, orrs_imm_32,
	mvn_imm_32, mvn_reg_32, b_imm_32, bc_imm_32,
	b_imm_16, bc_imm_16, mov_lit_16, ldr_imm_16, str_imm_16, mov_reg_reg_16
}opcode;

#define lim32 bc_imm_32

enum ArmCond{
    cond_eq = 0x0, cond_ne = 0x1,
    cond_lt = 0xB, cond_ge = 0xA,
    cond_al = 0xE
};

enum armFlags{
	flag_eq, flag_ne, flag_lt, flag_gt, flag_ge, flag_le
};

uint32_t progAddr;

void emitOpcode(const uint8_t code) {
	switch (code) {
	case movw_reg_reg_32: printf("MOVW_REG_REG_32"); break;
	case movw_lit_32:    printf("MOVW_LIT_32"); break;
	case movt_lit_32:    printf("MOVT_LIT_32"); break;
	case ldrw_imm_32:    printf("LDRW_IMM_32"); break;
	case strw_imm_32:    printf("STRW_IMM_32"); break;
	case ldrw_reg_32:    printf("LDRW_REG_32"); break;
	case strw_reg_32:    printf("STRW_REG_32"); break;
	case mov_lit_16:     printf("MOV_LIT_16"); break;
	case str_imm_16:     printf("STR_IMM_16"); break;
	case ldr_imm_16:     printf("LDR_IMM_16"); break;
	case subw_imm_32:    printf("SUBW_IMM_32"); break;
	case addw_imm_32:    printf("ADDW_IMM_32"); break;
	case subw_reg_32:    printf("SUBW_REG_32"); break;
	case addw_reg_32:    printf("ADDW_REG_32"); break;
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
	} progAddr += code > lim32 ? 2 : 4;
	printf("\n");
	fflush(stdout);
}

void emitFlag(const uint8_t flag) {
	switch(flag){
		case flag_gt: printf("FLAG GT\n"); break;
		case flag_ge: printf("FLAG GE\n"); break;
		case flag_eq: printf("FLAG EQ\n"); break;
		case flag_ne: printf("FLAG NE\n"); break;
		case flag_le: printf("FLAG LE\n"); break;
		case flag_lt: printf("FLAG LT\n");
	}
}

void emitModifier(const uint8_t mod){
	printf(mod ? "NO CARRY\n" : "CARRY\n");
}

void emitArgument(const uint32_t arg, const uint8_t sz) {
	printf("ARG %d\n", arg);
}

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

void storeConstantInReg(const uint8_t reg, const uint32_t c) {
	if(reg < 8 && c < UINT8_MAX){emitOpcode(mov_lit_16); emitArgument(reg, 3); emitArgument(c, 8);}
	else{
		emitOpcode(movw_lit_32); emitArgument(reg, 4); emitArgument(c, 16);
		if(c > 65535){emitOpcode(movt_lit_32); emitArgument(reg, 4); emitArgument(c, 16);}
	}
}
void storeRegisterIntoStack(const uint8_t reg, const uint32_t stackOffset) {
	if (stackOffset >= 4095) {
		storeConstantInReg(scratchReg1, stackOffset);
		emitOpcode(strw_reg_32); emitArgument(stackPointerReg, 4); emitArgument(reg, 4);
		emitArgument(scratchReg1, 4);
		return;
	}
	if(stackOffset < 1021 && stackOffset % 4 == 0 && reg < 8){emitOpcode(str_imm_16); emitArgument(reg, 3); emitArgument(stackOffset, 8);}
	else{
		emitOpcode(strw_imm_32); emitArgument(stackPointerReg, 4); emitArgument(reg, 4);
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
forceinline void storeRegisterIntoStackR(const uint8_t reg, const uint8_t reg2) {
	emitOpcode(strw_reg_32); emitArgument(stackPointerReg, 4); emitArgument(reg, 4);
	emitArgument(reg2, 4);
}
void loadRegisterFromStack(const uint8_t reg, const uint32_t stackOffset) {
	if (stackOffset >= 4095) {
		emitOpcode(movw_lit_32); emitArgument(scratchReg1, 4); emitArgument(stackOffset, 16);
		emitOpcode(movt_lit_32); emitArgument(scratchReg1, 4); emitArgument(stackOffset, 16);
		emitOpcode(ldrw_reg_32); emitArgument(stackPointerReg, 4); emitArgument(reg, 4);
		emitArgument(scratchReg1, 4);
		return;
	}
	if(stackOffset < 1021 && stackOffset % 4 == 0 && reg < 8){emitOpcode(ldr_imm_16); emitArgument(reg, 3); emitArgument(stackOffset, 8);}
	else{
		emitOpcode(ldrw_imm_32); emitArgument(stackPointerReg, 4); emitArgument(reg, 4);
		emitArgument(stackOffset, 12);
	}
}
void loadRegisterFromStackAbs(const uint8_t reg, const uint32_t stackOffset) {
	storeConstantInReg(scratchReg1, stackOffset);
	emitOpcode(ldrw_imm_32); emitArgument(scratchReg1, 4); emitArgument(reg, 4);
	emitArgument(0, 12);
}
forceinline void loadRegisterFromStackR(const uint8_t reg, const uint8_t reg2) {
	emitOpcode(ldrw_reg_32); emitArgument(stackPointerReg, 4); emitArgument(reg, 4);
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
void flushRegister(uint8_t regIdx) {
	if(virtualRegFile[regIdx].dirty == locked) return;
	if(virtualRegFile[regIdx].dirty == dirty) storeRegisterIntoStack(regIdx, virtualRegFile[regIdx].stackOffset); 
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
		loadRegisterFromStack(flushIdx, stackOffsetLoad);
	}
	virtualRegFile[flushIdx] = (registerData){stackOffsetStore, foundLoaded ? virtualRegFile[flushIdx].dirty : clean, priority};
	return flushIdx;
}

uint8_t moveConstantToRegs(const uint32_t val, const uint32_t stackOffsetStore, const uint16_t priority) {
	const uint8_t fIdx = getEmptyRegister(stackOffsetStore, priority, checkIfInRegs);
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
		else loadRegisterFromStack(flushIdx, stackOffsetLoad);
	}
	virtualRegFile[flushIdx] = (registerData){stackOffsetStore, clean, priority};
	return flushIdx;
}
forceinline void flushFlags(){
	if(virtualFlags.dirty != empty) moveFlagToRegs(virtualFlags.stackOffset, virtualFlags.stackOffset, virtualFlags.flagType, UINT16_MAX);
}

uint8_t moveOffsetToRegsFromRegister(const uint8_t loadRegister, const uint32_t stackOffsetStore, const uint16_t priority){
	uint16_t lowestPriority = UINT16_MAX; uint8_t foundEmpty = 0; uint8_t flushIdx = 0;
	for(uint8_t r = 0; r < maxGPRegs; r++){
		if(virtualRegFile[r].stackOffset == stackOffsetStore) virtualRegFile[r].dirty = empty;
		else if(virtualRegFile[r].dirty == empty){foundEmpty = 1; flushIdx = r;}
		else if(virtualRegFile[r].priority < lowestPriority && virtualRegFile[r].dirty != locked && !foundEmpty){lowestPriority = virtualRegFile[r].priority; flushIdx = r;}
	}
	if(!foundEmpty) flushRegister(flushIdx);
	loadRegisterFromStackR(flushIdx, loadRegister);
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
uint32_t curStackOffset = 0; uint8_t curScope = 0;
uint32_t curCompilerTempSz = 0;

variableMetadata addVariable(const char* name, const uint8_t strLen, const uint32_t size){
	variableBuffer[variableIdx] = (variableMetadata){curStackOffset, size, name, strLen, curScope}; 
	curStackOffset += size;
	return variableBuffer[variableIdx++];
}

forceinline void addCompilerTemp(const uint32_t sz){curStackOffset += sz; curCompilerTempSz += sz;}

void decrementScope(){
	curScope--; if (variableIdx == 0) { return; }
	for (variableMetadata* v = variableBuffer + variableIdx - 1; v >= variableBuffer && v->scope > curScope; v--) {
		variableIdx--;
		curStackOffset -= v->size;
	}
	curStackOffset -= curCompilerTempSz; curCompilerTempSz = 0;
}
forceinline void incrementScope(){curScope++;}

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
// OPERATION PARSER/HEX EMITTER

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
		case opAdd: return addw_imm_32;
		case opSub: return subw_imm_32;
		case opMul: return mulw_reg_32;
		case opBitwiseAnd: return andw_imm_32;
		case opBitwiseOr: return orrs_imm_32;
		case opBitwiseNot: return mvn_imm_32;
	}
} forceinline uint8_t returnRegOpcode(const uint8_t subtype){
	switch(subtype){
		case opAdd: return addw_reg_32;
		case opSub: return subw_reg_32;
		case opMul: return mulw_reg_32;
		case opBitwiseAnd: return andw_reg_32;
		case opBitwiseOr: return orrs_reg_32;
		case opBitwiseNot: return mvn_reg_32;
	}
}

forceinline uint8_t immSize(const uint8_t subtype){
	switch(subtype){
		case opBitwiseAnd: case opBitwiseXor: case opBitwiseOr: return 8;
		case opAdd: case opSub: case opBitwiseNot: return 16;
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
		case opWriteback: return 3;
		case opLogicalNot: case opBitwiseNot: case opReference: return 1;
		default: return 2;
	}
}

operand assembleOp(const operator op, const operand* operands, const uint16_t registerPermanence){
	operand ret, o1, o2;
	printf("\n");
	switch(op.subtype){
		case opEqual:{
		o2 = *operands; o1 = *(operands - 1);
		uint32_t num32BitTransfers = ((o1.size < o2.size ? o1.size : o2.size) + 3) >> 2; const uint32_t o1s = (o1.size + 3) >> 2;
		for(uint32_t wIdx = 0; wIdx < o1s; wIdx++){
			if(wIdx >= num32BitTransfers) {const uint8_t r = moveConstantToRegs(0, o1.val.stackOffset + wIdx * 4, registerPermanence); virtualRegFile[r].dirty = dirty; goto skipWriteback;}
			uint8_t r; switch(o2.operandType){
				case constant:
				virtualRegFile[r = moveConstantToRegs(o2.val.value, o1.val.stackOffset + wIdx * 4, UINT16_MAX)].dirty = dirty;
				goto skipWriteback;
				case stackVar:
				r = moveOffsetToRegs(o2.val.stackOffset + wIdx * 4, o2.val.stackOffset + wIdx * 4, registerPermanence);
				break;
				case flagVar:
				r = moveFlagToRegs(o2.val.stackOffset + wIdx * 4, o2.val.stackOffset + wIdx * 4, o2.flagType, registerPermanence);
			}
			if(!o1.forceFlush){
				if(o2.val.stackOffset + wIdx * 4 < curStackOffset - curCompilerTempSz || o2.val.stackOffset + wIdx * 4 >= curStackOffset){
					const uint8_t rr = getEmptyRegister(o1.val.stackOffset + wIdx * 4, registerPermanence, checkIfInRegs);
					virtualRegFile[rr].dirty = dirty; loadRegisterFromRegs(rr, r);
				} else{virtualRegFile[r].dirty = dirty; virtualRegFile[r].stackOffset = o1.val.stackOffset + wIdx * 4;}
			}
			skipWriteback:;
			if(o1.forceFlush){storeRegisterIntoStack(r, o1.val.stackOffset + wIdx * 4); virtualRegFile[r].dirty = empty;}
		}
		return o1;
		}
		case opReference:{
		o1 = *operands;
		curCompilerTempSz += 4; curStackOffset += 4;
		return (operand){o1.val.stackOffset, 4, constant, 0, 0, UINT16_MAX};
		}
		case opDereferenceVolatile: case opDereference:{
		o2 = *operands; o1 = *(operands - 1);
		uint32_t num32BitTransfers = o1.val.value;
		for(uint8_t r = 0; r < maxGPRegs; r++){
			if((virtualRegFile[r].stackOffset == o2.val.value && op.subtype == opDereferenceVolatile) || o2.operandType == stackVar)
			flushRegister(r);
		}
		do{
			num32BitTransfers--;
			switch(o2.operandType){
				case constant:
				virtualRegFile[moveOffsetToRegs(o2.val.value + num32BitTransfers * 4, curStackOffset + num32BitTransfers * 4, UINT16_MAX)].dirty = dirty;
				break;
				case stackVar:
				virtualRegFile[moveOffsetToRegsFromRegister(moveOffsetToRegs(o2.val.stackOffset + num32BitTransfers * 4, o2.val.stackOffset + num32BitTransfers * 4, UINT16_MAX), curStackOffset + num32BitTransfers * 4, UINT16_MAX)].dirty = dirty;
			}
			
		}while(num32BitTransfers > 0);
		curCompilerTempSz += 4; curStackOffset += 4;
		return (operand){curStackOffset - 4, o1.val.value, stackVar, 0, 0, registerPermanence};
		}
		case opWriteback:{
		// 1 deref 100 = 0;
		int8_t addrReg = -1;  uint32_t addrConst;
		const operand o3 = *operands; o2 = *(operands - 1); o1 = *(operands - 2);
		switch(o2.operandType){
			case constant:
			addrConst = o2.val.value; break;
			case stackVar:
			addrReg = moveOffsetToRegs(o2.val.stackOffset, o2.val.stackOffset, UINT16_MAX);
		}
		for(uint32_t wIdx = 0; wIdx < o1.val.value; wIdx++){
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
			if(addrReg > -1){storeRegisterIntoStackR(readReg, addrReg); if(wIdx < o1.val.value - 1){emitOpcode(addw_imm_32); emitArgument(addrReg, 4); emitArgument(addrReg, 4); emitArgument(4, 12);}}
			else storeRegisterIntoStack(readReg, addrConst + wIdx * 4);
		}
		if(addrReg > -1) virtualRegFile[addrReg].dirty = empty;
		for(uint8_t r = 0; r < maxGPRegs; r++){
			if(virtualRegFile[r].stackOffset == o2.val.value || o2.operandType == stackVar) virtualRegFile[r].dirty = empty;
		}
		curStackOffset += o1.val.value * 4; curCompilerTempSz += o1.val.value * 4;
		return (operand){curStackOffset - o1.val.value * 4, o1.val.value * 4, stackVar, 0, 0, UINT16_MAX};
		}
		case opSub: case opAdd: case opBitwiseAnd: case opBitwiseOr:{
		o2 = *operands, o1 = *(operands - 1);
		const uint32_t num32BitAdds = ((o1.size > o2.size ? o1.size : o2.size) + 3) >> 2;
		for(uint32_t wIdx = 0; wIdx < num32BitAdds; wIdx++){
			int64_t constantInAdd = -1; int8_t r1 = -1; uint8_t r1d, r2d;
			switch(o1.operandType){
				case constant:
				constantInAdd = wIdx ? 0 : o1.val.value;
				if(o1.val.value >= 2 << immSize(op.subtype) && o2.operandType != constant && !wIdx) r1 = moveConstantToRegs(o1.val.value, curStackOffset - curCompilerTempSz + 1, UINT16_MAX);
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
				if(!wIdx){constantInAdd = o2.val.value; if(o2.val.value >= 2 << immSize(op.subtype)) r2 = moveConstantToRegs(o2.val.value, curStackOffset - curCompilerTempSz + 1, UINT16_MAX);}
				else constantInAdd = 0;
				break;
				case stackVar:
				if(r1 == -1){
					r1 = moveOffsetToRegs(o2.val.stackOffset + wIdx * 4, o2.val.stackOffset + wIdx * 4, o2.registerPreference);
					virtualRegFile[r1].dirty = locked; r1d = virtualRegFile[r1].dirty;
				} else {
					r2 = moveOffsetToRegs(o2.val.stackOffset + wIdx * 4, o2.val.stackOffset + wIdx * 4, o2.registerPreference);
					virtualRegFile[r2].dirty = locked; r2d = virtualRegFile[r2].dirty;
				}
			}
			if(virtualRegFile[r1].stackOffset >= curStackOffset - curCompilerTempSz && virtualRegFile[r1].stackOffset < curStackOffset)
			virtualRegFile[r1].dirty = empty; else virtualRegFile[r1].dirty = r1d;
			if(r2 != -1 && virtualRegFile[r2].stackOffset >= curStackOffset - curCompilerTempSz && virtualRegFile[r2].stackOffset < curStackOffset)
			virtualRegFile[r2].dirty = empty; else virtualRegFile[r2].dirty = r2d;
			const uint8_t resultReg = getEmptyRegister(curStackOffset + wIdx * 4, registerPermanence, noCheck); virtualRegFile[resultReg].dirty = dirty;
			if(r2 == -1) emitOpcode(returnImmOpcode(op.subtype)); else emitOpcode(returnRegOpcode(op.subtype));
			emitModifier(wIdx == 0);
			emitArgument(resultReg, 4); emitArgument(r1, 4); emitArgument(r2 == -1 ? constantInAdd : r2, r2 == -1 ? immSize(op.subtype) : 4);
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
				if(o1.val.value >= UINT8_MAX) virtualRegFile[r1 = moveConstantToRegs(o1.val.value, curStackOffset - curCompilerTempSz + 1, UINT16_MAX), r1].dirty = locked;
				else constantInCmp = o1.val.value; break;
				case stackVar:
				r1 = moveOffsetToRegs(o1.val.stackOffset + wIdx * 4, o1.val.stackOffset + wIdx * 4, o1.registerPreference);
				r1d = virtualRegFile[r1].dirty;
				break;
			} if(r1 != -1) virtualRegFile[r1].dirty = locked;
			int8_t r2 = -1;
			switch(o2.operandType){
				case constant:
				if(!wIdx){constantInCmp = o2.val.value; if(o2.val.value >= UINT8_MAX) r2 = moveConstantToRegs(o2.val.value, curStackOffset - curCompilerTempSz + 1, UINT16_MAX);}
				else constantInCmp = 0;
				break;
				case stackVar:
				if(r1 == -1){
					r1 = moveOffsetToRegs(o2.val.stackOffset + wIdx * 4, o2.val.stackOffset + wIdx * 4, o2.registerPreference);
					virtualRegFile[r1].dirty = locked; r1d = virtualRegFile[r1].dirty;
				} else {
					r2 = moveOffsetToRegs(o2.val.stackOffset + wIdx * 4, o2.val.stackOffset + wIdx * 4, o2.registerPreference);
					virtualRegFile[r2].dirty = locked; r2d = virtualRegFile[r2].dirty;
				}
			}
			if(virtualRegFile[r1].stackOffset >= curStackOffset - curCompilerTempSz && virtualRegFile[r1].stackOffset < curStackOffset)
			virtualRegFile[r1].dirty = empty; else virtualRegFile[r1].dirty = r1d;
			if(r2 != -1){ if(virtualRegFile[r2].stackOffset >= curStackOffset - curCompilerTempSz && virtualRegFile[r2].stackOffset < curStackOffset)
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
			virtualRegFile[r1].dirty = (virtualRegFile[r1].stackOffset >= curStackOffset - curCompilerTempSz && virtualRegFile[r1].stackOffset < curStackOffset) ? empty : r1d;
		}
		if(r2 != -1){
			emitOpcode(cmp_imm_32); emitArgument(r2, 4); emitArgument(0, 12);
			virtualRegFile[r2].dirty = (virtualRegFile[r2].stackOffset >= curStackOffset - curCompilerTempSz && virtualRegFile[r2].stackOffset < curStackOffset) ? empty : virtualRegFile[r2].dirty;
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
			r1d = virtualRegFile[r1].dirty = locked;
		} switch(o2.operandType){
			case constant: if(constantInMul != -1) return (operand){evalImm(constantInMul, o2.val.value, op.subtype), 4, constant, 0, 0, registerPermanence};
			constantInMul = o2.val.value;
			case stackVar: 
			if(r1 == -1){
				r1 = moveOffsetToRegs(o2.val.stackOffset, o2.val.stackOffset, registerPermanence);
				r1d = virtualRegFile[r1].dirty; virtualRegFile[r1].dirty = locked;
			}
			else{
				r2 = moveOffsetToRegs(o2.val.stackOffset, o2.val.stackOffset, registerPermanence);
				r2d = virtualRegFile[r2].dirty; virtualRegFile[r2].dirty = locked;
			}
		}
		if(constantInMul != -1) r2 = moveConstantToRegs(constantInMul, UINT32_MAX, UINT16_MAX);
		if(virtualRegFile[r1].stackOffset >= curStackOffset - curCompilerTempSz && virtualRegFile[r1].stackOffset < curStackOffset) virtualRegFile[r1].dirty = empty;
		if(virtualRegFile[r2].stackOffset >= curStackOffset - curCompilerTempSz && virtualRegFile[r2].stackOffset < curStackOffset) virtualRegFile[r2].dirty = empty;
		const uint8_t resultReg = getEmptyRegister(curStackOffset, registerPermanence, noCheck); virtualRegFile[resultReg].dirty = dirty; virtualRegFile[resultReg].stackOffset = curStackOffset;
		if(virtualRegFile[r1].dirty == locked) virtualRegFile[r1].dirty = r1d; if(virtualRegFile[r2].dirty == locked) virtualRegFile[r2].dirty = r2d;
		emitOpcode(op.subtype == opMul ? mulw_reg_32 : divw_reg_32); emitArgument(resultReg, 4); emitArgument(r1, 4); emitArgument(r2, 4);
		curStackOffset += 4;
		return (operand){curStackOffset - 4, 4, stackVar, 0, 0, registerPermanence};	
		}
	}
}

//#############################################################################################################################################
// EXPRESSION PARSER

#define maxOperatorDepth 128
#define maxOperands 256
#define maxBranchDepth 64

// comments for clarity here, sorry for the confusion with precedences.
const uint8_t operatorPrecedence[] = { 
    4, 4, 5, 5, // Add, Sub, Mul, Div
    1,          // Equal (Assignment)
    8, 8, 8,    // Reference(Unary), Writeback, Dereference
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
				if(r2 != r){flushRegister(r); loadRegisterFromRegs(r, r2); virtualRegFile[r].dirty = clean; virtualRegFile[r2].dirty = empty;} goto continueLoop; 
			}
			flushRegister(r); loadRegisterFromStack(r, snapshot.registerStatus[r]); virtualRegFile[r].dirty = clean;
		} else flushRegister(r);
		continueLoop:;
	}
}

registerSnapshot snapshotStack[maxBranchDepth]; uint32_t relativeBranchOffsets[maxBranchDepth];
operator operatorStack[maxOperatorDepth]; operand operandStack[maxOperands];
void assembleSource(const char* src, const uint32_t progOrigin){
	initializeVirtualRegs(); curScope = 0;
	progAddr = 0; setSource(src); uint8_t operatorIdx = 0; uint8_t operandIdx = 0;
	uint8_t registerPermanence = 0; uint32_t allocationSz = 0; uint8_t allocationFound = 0, flushFound = 0, volatileFound = 0;
	int8_t branchFound = -1; uint32_t jmpBackAddr; uint8_t precedence = 0; virtualFlags.dirty = empty;
	curToken.type = opToken;
	while(curToken.type != nullToken){
		nextToken(); const uint32_t tmpSz = allocationSz; const uint8_t tmpFound = allocationFound;
		allocationSz = 0; allocationFound = 0;
		switch(curToken.type){
			case constantToken:
			allocationSz = tmpSz; allocationFound = tmpFound ? 1 : 0;
			uint32_t literal = 0;
			for(uint8_t digit = 0; digit < curToken.len; digit++){
				literal *= 10;
				literal += curToken.str[digit] - '0';
			}
			if(allocationFound){allocationFound = 0; allocationSz = literal; literal *= 4;}
			operandStack[operandIdx++] = (operand){literal, 4, constant, 0, 0, registerPermanence};
			break;
			case sizeToken:
			allocationFound = 2; allocationSz = 1;
			break;
			case keywordToken:
			switch(curToken.subtype){
				case flushKey:
				flushFound = 1;
				break;
				case ifKey: case whileKey:
				branchFound = curToken.subtype; break;
				case volatileKey: volatileFound = 1; break;
			}
			break;
			case identifierToken:
			allocationSz = tmpSz; allocationFound = tmpFound;
			if(allocationSz > 0){
				if(allocationFound != 2) operandIdx--;
				const variableMetadata v = addVariable(curToken.str, curToken.len, allocationSz * 4);
				operandStack[operandIdx++] = (operand){v.stackOffset, allocationSz * 4, stackVar, 0, flushFound, registerPermanence};
				allocationSz = 0;
			}
			else{
				const variableMetadata v = retrieveLocalVariable(curToken.str, curToken.len);
				operandStack[operandIdx++] = (operand){v.stackOffset, v.size, stackVar, 0, flushFound, registerPermanence};
			}
			allocationFound = 0;
			flushFound = 0;
			break;
			case opToken:
			const uint32_t curPrecedence = operatorPrecedence[curToken.subtype] + precedence;
			if(volatileFound && curToken.subtype == opDereference) curToken.subtype = opDereferenceVolatile;
			while(1){
				uint32_t prevPrecedence = operatorIdx > 0 ? operatorStack[operatorIdx-1].precedence : 0;
				if(curPrecedence > prevPrecedence || operatorIdx == 1) break;
				const operand tmp = assembleOp(operatorStack[--operatorIdx], operandStack + operandIdx - 1, registerPermanence);
				const uint8_t oIdx = operatorStack[operatorIdx].subtype; operandIdx -= numOperands(oIdx);
				operandStack[operandIdx++] = tmp;
			} operatorStack[operatorIdx++] = (operator){curToken.subtype, curPrecedence};
			break;
			case clampToken:
			switch(curToken.subtype){
				case parenthesesL: case parenthesesR:
				precedence += (curToken.subtype == parenthesesL) * 10 - (curToken.subtype == parenthesesR) * 10;
				break;	
				case curlyBL: goto endLineG;
				case curlyBR:
				decrementScope(); 
				switch(branchFound){
					case ifKey: break;
					case whileKey: emitOpcode(b_imm_32); emitArgument(relativeBranchOffsets[curScope] - progAddr, 16);
				}
				for(uint8_t r = 0; r < maxGPRegs; r++) if(virtualRegFile[r].stackOffset >= curStackOffset) virtualRegFile[r].dirty = empty;
				restoreSnapshot(snapshotStack[curScope]);
				printf("BACKPATCH %d\n", progAddr - relativeBranchOffsets[curScope]); 
				branchFound = -1;
			}
			break;
			case endLine:
			endLineG:;
			for(int8_t idx = operatorIdx - 1; idx >= 0; idx--){
				if(idx > 0){
					if(operatorStack[idx].subtype == opEqual && operatorStack[idx-1].subtype == opDereference) operatorStack[--idx].subtype = opWriteback;
					else if(operatorStack[idx].subtype == opCmpEqual){
						switch(operatorStack[idx-1].subtype){
							case opLogicalNot: operatorStack[--idx].subtype = opCmpNEqual; break;
							case opCmpGreater: operatorStack[--idx].subtype = opCmpGEqual; break;
							case opCmpLess: operatorStack[--idx].subtype = opCmpLEqual; break;
						}
					}
				}
				const operand tmp = assembleOp(operatorStack[idx], operandStack + operandIdx - 1, registerPermanence);
				operandIdx -= numOperands(operatorStack[idx].subtype);
				operandStack[operandIdx++] = tmp;
			}
			if(curToken.subtype == curlyBL){ 
				switch(branchFound){
				case whileKey: relativeBranchOffsets[curScope] = progAddr;
				case ifKey:
				const operand condition = operandStack[--operandIdx];
				switch(condition.operandType){
					case flagVar:
					// nothing yet
					break;
					break;
					case constant:
					if(!condition.val.value){
						uint8_t depth = 1;
						while(depth){
							nextToken();
							if(curToken.type == clampToken) depth += (curToken.subtype == curlyBL) - (curToken.subtype == curlyBR);
						}
					}continue;
					goto skpCondB;
					case stackVar:
					operandStack[operandIdx++] = condition; operandStack[operandIdx] = (operand){0, 4, constant, 0, 0, registerPermanence};
					assembleOp((operator){opCmpEqual, 0}, operandStack + operandIdx, registerPermanence);
				}
				if(branchFound == ifKey) relativeBranchOffsets[curScope] = progAddr;
				registerPermanence += (branchFound == whileKey) * 128;
				emitOpcode(bc_imm_32); emitFlag(getOppositeFlag(virtualFlags.flagType)); emitArgument(4096, 12);
				skpCondB:;
				}snapshotStack[curScope] = getSnapshot(); incrementScope();
			}
			operatorIdx = 0;
			for(uint8_t r = 0; r < maxGPRegs; r++){
				if(virtualRegFile[r].stackOffset > curStackOffset - curCompilerTempSz && virtualRegFile[r].stackOffset < curStackOffset){
					virtualRegFile[r].dirty = empty;
				}
			}
			curStackOffset -= curCompilerTempSz; curCompilerTempSz = 0;
			break;
		}
	}
}