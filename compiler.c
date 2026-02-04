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

//#############################################################################################################################################
// TOKENIZER

enum tokenType {
	opToken, identifierToken, keywordToken, constantToken,
	sizeToken, clampToken, endLine, nullToken
};

enum opSubtype {
	opAdd, opSub, opMul, opDiv, opEqual, opReference, opWriteback,
	opDereference, opCmpGreater, opCmpLess, opCmpEqual,
	opBitwiseOr, opBitwiseAnd, opBitwiseNot, opLogicalAnd,
	opLogicalOr, opLogicalNot, opPointerCast
};

enum clampSubtype {
	parenthesesL, parenthesesR,
	curlyBL, curlyBR
};

enum keywordSubtype {
	ifKey, forKey, whileKey,
	breakKey, continueKey, globalKey
};

typedef struct{
	char* str; uint16_t len;
	uint8_t type, subtype;
}token;

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
void setSource(const char* c){src = c;}

void nextToken(){
	while(*src == ' ' || *src == '\n')src++;
	curToken = (token){(void*)0, 0, nullToken, 0}; if(*src == '\0')return;
	curToken.str = src; curToken.len = 0;
	if(isSingle(*src)){src++; curToken.len++;}
	else{while(*src != ' ' && !isSingle(*src) && *src != '\n'){src++; curToken.len++;}}
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
		case 'i': if(tokenCmpLiteral(curToken, "if")){curToken.type = keywordToken; curToken.subtype = ifKey;}
		else{curToken.type = identifierToken; curToken.subtype = 0;} break;
		case 'f': if(tokenCmpLiteral(curToken, "for")){curToken.type = keywordToken; curToken.subtype = forKey;}
		else{curToken.type = identifierToken; curToken.subtype = 0;} break;
		case 'c': if(tokenCmpLiteral(curToken, "continue")){curToken.type = keywordToken; curToken.subtype = continueKey;}
		else{curToken.type = identifierToken; curToken.subtype = 0;} break;
		case 'b': if(tokenCmpLiteral(curToken, "break")){curToken.type = keywordToken; curToken.subtype = breakKey;}
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
	mov_reg_reg_32, movs, movw_lit_32, movt_lit_32,
	ldrw_imm_32, strw_imm_32, ldrw_reg_32, strw_reg_32,
	subw_imm_32, addw_imm_32, subw_reg_32, addw_reg_32,
	mulw_reg_32, divw_reg_32, ite_32, it_32,
	cmp_reg_32, cmp_imm_32, eors_reg_32, eors_imm_32, orrs_reg_32
}opcode;

enum ArmCond{
    cond_eq = 0x0, cond_ne = 0x1,
    cond_lt = 0xB, cond_ge = 0xA,
    cond_al = 0xE
};

enum armFlags{
	flag_eq, flag_ne, flag_lt, flag_gt
};

void emitOpcode(const uint8_t code) {
	switch (code) {
	case mov_reg_reg_32: printf("MOV_REG_REG_32");    break;
	case movs:           printf("MOVS");         break;
	case movw_lit_32:    printf("MOVW_LIT_32");         break;
	case movt_lit_32:    printf("MOVT_LIT_32");         break;
	case ldrw_imm_32:    printf("LDRW_IMM_32");    break;
	case strw_imm_32:    printf("STRW_IMM_32");    break;
	case ldrw_reg_32:    printf("LDRW_REG_32");    break;
	case strw_reg_32:    printf("STRW_REG_32");    break;
	case subw_imm_32:    printf("SUBW_IMM_32");   break;
	case addw_imm_32:    printf("ADDW_IMM_32");   break;
	case subw_reg_32:    printf("SUBW_REG_32");   break;
	case addw_reg_32:    printf("ADDW_REG_32");   break;
	case mulw_reg_32:    printf("MULW_REG_32");   break;
	case divw_reg_32:    printf("DIVW_REG_32");   break;
	case eors_imm_32:	 printf("EORS_IMM_32"); break;
	case eors_reg_32:	 printf("EORS_REG_32"); break;
	case orrs_reg_32:	 printf("ORRS_REG_32"); break;
	case ite_32:		 printf("ITE_32"); break;
	case it_32:		 	 printf("IT_32"); break;
	default:             printf("UNKNOWN_OP");   break;
	}
	printf("\n");
	fflush(stdout);
}

void emitFlag(const uint8_t flag) {
	printf(!flag ? "CARRY\n" : "NO CARRY\n");
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
	uint8_t dirty;
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
	emitOpcode(movw_lit_32); emitArgument(reg, 4); emitArgument(c, 16);
	if(c > 65535){emitOpcode(movt_lit_32); emitArgument(reg, 4); emitArgument(c, 16);}
}
void storeRegisterIntoStack(const uint8_t reg, const uint32_t stackOffset) {
	if (stackOffset >= 4095) {
		storeConstantInReg(scratchReg1, stackOffset);
		emitOpcode(strw_reg_32); emitArgument(stackPointerReg, 4); emitArgument(reg, 4);
		emitArgument(scratchReg1, 4);
		return;
	}
	emitOpcode(strw_imm_32); emitArgument(stackPointerReg, 4); emitArgument(reg, 4);
	emitArgument(stackOffset, 12);
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
	emitOpcode(ldrw_imm_32); emitArgument(stackPointerReg, 4); emitArgument(reg, 4);
	emitArgument(stackOffset, 12);
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
	emitOpcode(ite_32); emitFlag(flag); emitOpcode(movw_lit_32); emitArgument(reg, 4); emitArgument(1, 16);
	emitOpcode(movw_lit_32); emitArgument(reg, 4); emitArgument(0, 16);
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
		if(virtualRegFile[r].dirty == empty){flushIdx = r; foundEmpty = 1;}
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
		if(virtualRegFile[r].stackOffset == stackOffsetLoad && stackOffsetLoad != UINT32_MAX && virtualRegFile[r].dirty != empty){foundLoaded = 1; flushIdx = r;} 
		else if(virtualRegFile[r].stackOffset == stackOffsetStore) virtualRegFile[r].dirty = empty;
		else if(virtualRegFile[r].dirty == empty){foundEmpty = 1; flushIdx = r;}
		else if(virtualRegFile[r].priority < lowestPriority && virtualRegFile[r].dirty != locked && !foundEmpty && !foundLoaded){lowestPriority = virtualRegFile[r].priority; flushIdx = r;}
	}
	if(!foundLoaded){
		if(!foundEmpty) flushRegister(flushIdx);
		loadRegisterFromStack(flushIdx, stackOffsetLoad);
	}
	virtualRegFile[flushIdx] = (registerData){stackOffsetStore, clean, priority};
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
		else if(virtualRegFile[r].dirty == empty){foundEmpty = 1; flushIdx = r;}
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
	uint8_t operandType; uint8_t flagType; uint16_t registerPreference;
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
	curStackOffset += size; curStackOffset = (curStackOffset + 3) & ~3;
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

operand assembleOp(const operator op, const operand* operands, const uint16_t registerPermanence){
	operand ret, o1, o2;
	printf("\n");
	switch(op.subtype){
		case opEqual:{
		o2 = *operands; o1 = *(operands - 1);
		uint32_t num32BitTransfers = ((o1.size < o2.size ? o1.size : o2.size) + 3) >> 2; const uint32_t o1s = (o1.size + 3) >> 2;
		for(uint32_t wIdx = 0; wIdx < o1s; wIdx++){
			if(wIdx >= num32BitTransfers) {virtualRegFile[moveConstantToRegs(0, o1.val.stackOffset + wIdx * 4, registerPermanence)]; continue;}
			switch(o2.operandType){
				case constant:
				virtualRegFile[moveConstantToRegs(o2.val.value, o1.val.stackOffset + wIdx * 4, UINT16_MAX)].dirty = dirty;
				break;
				case stackVar:
				virtualRegFile[moveOffsetToRegs(o2.val.stackOffset + wIdx * 4, o1.val.stackOffset + wIdx * 4, UINT16_MAX)].dirty = dirty;
				break;
				case flagVar:
				virtualRegFile[moveFlagToRegs(o2.val.stackOffset + wIdx * 4, o1.val.stackOffset + wIdx * 4, o2.flagType, registerPermanence)].dirty = dirty;
				for(uint8_t r = 0; r < maxGPRegs; r++)
				if(virtualRegFile[r].stackOffset >= o1.val.stackOffset && virtualRegFile[r].stackOffset < o1.val.stackOffset + o1.size){virtualRegFile[r].dirty = empty;}
				o1.operandType = flagVar; o1.flagType = o2.flagType;
			}
		}
		return o1;
		}
		case opReference:{
		o1 = *operands;
		curCompilerTempSz += 4;
		for(uint8_t r = 0; r < maxGPRegs; r++){
			if(virtualRegFile[r].stackOffset >= o1.val.stackOffset && virtualRegFile[r].stackOffset < o1.val.stackOffset + o1.size) flushRegister(r);
		}
		return (operand){o1.val.stackOffset, 4, constant, UINT16_MAX};
		}
		case opDereference:{
		o2 = *operands; o1 = *(operands - 1);
		uint32_t num32BitTransfers = o1.val.value;
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
		return (operand){curStackOffset - 4, o1.val.value, stackVar, registerPermanence};
		}
		case opWriteback:{
		// 1 deref 100 = 0;
		const operand o3 = *operands; o2 = *(operands - 1); o1 = *(operands - 2);
		uint32_t num32BitTransfers = ((o1.val.value < o3.size ? o1.val.value : o3.size) + 3) >> 2;
		#define nullRegister 100
		uint8_t addrReg = nullRegister;
		const uint8_t tempReg = getEmptyRegister(0, registerPermanence, noCheck);
		for(uint8_t r = 0; r < maxGPRegs; r++) flushRegister(r);
		switch(o2.operandType){
			case stackVar:{
			addrReg = moveOffsetToRegs(o2.val.stackOffset, UINT32_MAX, UINT16_MAX);
			virtualRegFile[addrReg].dirty = locked;
			break;
			default:;
			}
		}
		uint32_t idx = 0;
		while(1){
			switch(o3.operandType){
				case stackVar:
				loadRegisterFromStack(tempReg, o3.val.stackOffset + idx * 4);
				break;
				case constant:
				storeConstantInReg(tempReg, idx == 0 ? o3.val.value : 0);
			}
			if(addrReg != nullRegister){	
				storeRegisterIntoStackR(tempReg, addrReg);
				idx++; if(idx == num32BitTransfers) break;
				emitOpcode(addw_imm_32); emitArgument(addrReg, 4); emitArgument(addrReg, 4); emitArgument(1, 12);
				continue;
			}
			storeRegisterIntoStack(tempReg, o2.val.value + idx * 4);
			idx++; if(idx == num32BitTransfers) break;
		}
		curCompilerTempSz += num32BitTransfers * 4; curStackOffset += num32BitTransfers * 4;
		virtualRegFile[tempReg].dirty = empty;
		return (operand){curStackOffset - num32BitTransfers * 4, o1.val.value, stackVar, UINT16_MAX};
		}
		case opSub: case opAdd:{
		o2 = *operands, o1 = *(operands - 1);
		const uint32_t num32BitAdds = ((o1.size > o2.size ? o1.size : o2.size) + 3) >> 2;
		for(uint32_t wIdx = 0; wIdx < num32BitAdds; wIdx++){
			uint32_t constantInAdd = 0;
			int8_t r1 = -1; const uint8_t o1Zero = o1.size <= wIdx * 4;
			if(!o1Zero) switch(o1.operandType){
				case constant: if(o1.val.value < 4096) constantInAdd = o1.val.value;
				else r1 = moveConstantToRegs(o1.val.value, UINT32_MAX, UINT16_MAX);
				break;
				case stackVar:
				r1 = moveOffsetToRegs(o1.val.stackOffset + wIdx * 4, o1.val.stackOffset + wIdx * 4, o1.registerPreference);
				break;
				case flagVar:
				if(virtualFlags.stackOffset != o1.val.stackOffset) r1 = moveFlagToRegs(o1.val.stackOffset, o1.val.stackOffset, o1.flagType, o1.registerPreference);
				break;
			} if(r1 != -1) virtualRegFile[r1].dirty = locked;
			int8_t r2 = -1; const uint8_t o2Zero = o2.size <= wIdx * 4;
			if(!o2Zero) switch(o2.operandType){
				case constant: if(o2.val.value < 4096) constantInAdd = o2.val.value;
				else r2 = moveConstantToRegs(o2.val.value, UINT32_MAX, UINT16_MAX);
				break;
				case stackVar:
				r2 = moveOffsetToRegs(o2.val.stackOffset + wIdx * 4, o2.val.stackOffset + wIdx * 4, o2.registerPreference);
				break;
				case flagVar:
				if(virtualFlags.stackOffset != o2.val.stackOffset) r2 = moveFlagToRegs(o2.val.stackOffset, o2.val.stackOffset, o2.flagType, o2.registerPreference);
				break;
			}
			if(o1.val.stackOffset >= curStackOffset - curCompilerTempSz && r1 != -1) virtualRegFile[r1].dirty = empty;
			if(o2.val.stackOffset >= curStackOffset - curCompilerTempSz && r2 != -1) virtualRegFile[r2].dirty = empty;
			const uint8_t resultReg = getEmptyRegister(curStackOffset + wIdx * 4, UINT16_MAX, noCheck);
			uint8_t foundFlag = 0;
			if(r1 != -1 && r2 != -1){emitOpcode(op.subtype == opAdd ? addw_reg_32 : subw_reg_32); goto skpAddwImm;}
			else if(r1 == -1 && r2 == -1){return (operand){o1.val.value + (op.subtype == opAdd ? o2.val.value : (-o2.val.value)), 4, constant, registerPermanence};}
			else if(r1 == -1 && o1.operandType == flagVar){foundFlag = 1; emitOpcode(it_32); emitArgument(o1.flagType, 4);}
			else if(r2 == -1 && o2.operandType == flagVar){foundFlag = 1; emitOpcode(it_32); emitArgument(o2.flagType, 4);}
			emitOpcode(op.subtype == opAdd ? addw_imm_32 : subw_imm_32); skpAddwImm:;
			emitArgument(resultReg, 4); emitArgument(r1 == -1 ? r2 : r1, 4);
			emitArgument((r1 != -1 && r2 != -1) ? r2 : (foundFlag ? 1 : constantInAdd), (r1 == -1 || r2 == -1) ? 12 : 4);
		}
		curStackOffset += num32BitAdds * 4; curCompilerTempSz += num32BitAdds * 4;
		return (operand){curStackOffset - num32BitAdds * 4, num32BitAdds * 4, stackVar, registerPermanence};
		break;
		}
		case opCmpEqual:{
			o2 = *operands, o1 = *(operands - 1);
			const uint32_t num32BitComparisons = ((o1.size > o2.size ? o1.size : o2.size) + 3) >> 2;
			const uint8_t accumulationReg = getEmptyRegister(UINT32_MAX, UINT16_MAX, noCheck);
			storeConstantInReg(accumulationReg, 0);
			for(uint32_t wIdx = 0; wIdx < num32BitComparisons; wIdx++){
				uint32_t constantInCmp = 0;
				int8_t r1 = -1; const uint8_t o1Zero = o1.size <= wIdx * 4;
				if(!o1Zero) switch(o1.operandType){
					case constant: if(o1.val.value < 4096) constantInCmp = o1.val.value;
					else r1 = moveConstantToRegs(o1.val.value, UINT32_MAX, UINT16_MAX);
					break;
					case stackVar:
					r1 = moveOffsetToRegs(o1.val.stackOffset + wIdx * 4, o1.val.stackOffset + wIdx * 4, o1.registerPreference);
				} if(r1 != -1) virtualRegFile[r1].dirty = locked;
				int8_t r2 = -1; const uint8_t o2Zero = o2.size <= wIdx * 4;
				if(!o2Zero) switch(o2.operandType){
					case constant: if(o2.val.value < 4096) constantInCmp = o2.val.value;
					else r2 = moveConstantToRegs(o2.val.value, UINT32_MAX, UINT16_MAX);
					break;
					case stackVar:
					r2 = moveOffsetToRegs(o2.val.stackOffset + wIdx * 4, o2.val.stackOffset + wIdx * 4, o2.registerPreference);
				}
				if(o1.val.stackOffset >= curStackOffset - curCompilerTempSz && r1 != -1 && o1.operandType != constant) virtualRegFile[r1].dirty = empty;
				if(o2.val.stackOffset >= curStackOffset - curCompilerTempSz && r2 != -1 && o2.operandType != constant) virtualRegFile[r2].dirty = empty;
				const uint8_t resultReg = getEmptyRegister(UINT32_MAX, UINT16_MAX, noCheck); virtualRegFile[resultReg].dirty = empty;
				if(r1 != -1 && r2 != -1){emitOpcode(eors_reg_32); goto skpCmpImm;}
				else if(r1 == -1 && r2 == -1) return (operand){o1.val.value == o2.val.value, 4, constant, registerPermanence};
				emitOpcode(eors_imm_32); skpCmpImm:;
				emitArgument(resultReg, 4); emitArgument(r1 != -1 ? r1 : r2, 4);
				emitArgument(r2 == -1 ? (o1Zero || o2Zero ? 0 : constantInCmp) : r2, r2 == -1 ? 12 : 4);
				emitOpcode(orrs_reg_32); emitArgument(accumulationReg, 4); emitArgument(resultReg, 4); emitArgument(accumulationReg, 4);
			}
			virtualFlags.dirty = dirty; virtualFlags.stackOffset = curStackOffset;
			curStackOffset += 4; curCompilerTempSz += 4;
			virtualRegFile[accumulationReg].dirty = empty;
			return (operand){curStackOffset - 4, 4, flagVar, registerPermanence};
			break;
		}
	}
}

//#############################################################################################################################################
// EXPRESSION PARSER

#define maxOperatorDepth 100
#define maxOperands 200

const uint8_t operatorPrecedence[] = { 3, 3, 4, 4, 1, 7, 7, 2, 2, 2, 5, 5, 6, 5, 5, 6, 8 };

void assembleSource(){
	initializeVirtualRegs();
	operator operatorStack[maxOperatorDepth]; uint8_t operatorIdx = 0;
	operand operandStack[maxOperands]; uint8_t operandIdx = 0;
	uint8_t registerPermanence = 0;
	uint32_t allocationSz = 0; uint8_t allocationFound = 0;
	uint8_t precedence = 0;
	curToken.type = opToken;
	while(curToken.type != nullToken){
		nextToken();	
		switch(curToken.type){
			case constantToken:
			uint32_t literal = 0;
			for(uint8_t digit = 0; digit < curToken.len; digit++){
				literal *= 10;
				literal += curToken.str[digit] - '0';
			}
			if(allocationFound){allocationFound = 0; allocationSz = literal; break;}
			operandStack[operandIdx++] = (operand){literal, 4, constant, 0, registerPermanence};
			break;
			case sizeToken:
			allocationFound = 1;
			break;
			case identifierToken:
			if(allocationSz > 0){
				const variableMetadata v = addVariable(curToken.str, curToken.len, allocationSz * 4);
				operandStack[operandIdx++] = (operand){v.stackOffset, allocationSz * 4, stackVar, registerPermanence};
				allocationSz = 0;
				
			}
			else{
				const variableMetadata v = retrieveLocalVariable(curToken.str, curToken.len);
				operandStack[operandIdx++] = (operand){v.stackOffset, v.size, stackVar, registerPermanence};
			}
			break;
			case opToken:
			if(operatorIdx > 0){ if(operatorStack[operatorIdx - 1].subtype == opDereference && curToken.subtype == opEqual){
				operatorStack[operatorIdx - 1].subtype = opWriteback; break;
			}}
			const uint32_t curPrecedence = operatorPrecedence[curToken.subtype] + precedence;
			while(1){
				uint32_t prevPrecedence = operatorIdx > 0 ? operatorStack[operatorIdx-1].precedence : 0;
				if(curPrecedence > prevPrecedence) break;
				const operand tmp = assembleOp(operatorStack[--operatorIdx], operandStack + operandIdx - 1, registerPermanence);
				operandIdx -= operatorStack[operatorIdx].subtype == opReference ? 1 : 2;
				operandStack[operandIdx++] = tmp;
			} operatorStack[operatorIdx++] = (operator){curToken.subtype, curPrecedence};
			break;
			case clampToken:
			precedence += (curToken.subtype == parenthesesL) * 10 - (curToken.subtype == parenthesesR) * 10;
			break;
			case endLine:
			for(int8_t idx = operatorIdx - 1; idx >= 0; idx--){
				const operand tmp = assembleOp(operatorStack[idx], operandStack + operandIdx - 1, registerPermanence);
				operandIdx -= operatorStack[idx].subtype == opReference ? 1 : 2;
				operandStack[operandIdx++] = tmp;
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