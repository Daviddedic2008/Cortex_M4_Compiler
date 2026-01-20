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

//#############################################################################################################################################
// TOKENIZER

enum tokenType {
	opToken, identifierToken, keywordToken, constantToken,
	sizeToken, clampToken, endLine, nullToken
};

enum opSubtype {
	opAdd, opSub, opMul, opDiv, opEqual, opReference,
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
	char* tmp = s.str;
	for (; *lit != '\0' && (s.str - tmp) < s.len; lit++, s.str++) {
		if (*s.str != *lit) { return 0; }
	}
	return 1;
}

char* src; token curToken;
void setSource(const char* c){src = c;}

void nextToken(){
	while(*src == ' ' || *src == '\n')src++;
	curToken = (token){(void*)0, 0, nullToken, 0}; if(*src == '\0')return;
	curToken.str = src; curToken.len = 0;
	if(isSingle(*src)){src++; curToken.len++;}
	else{while(*src != ' ' && !isSingle(*src) && *src != '\n'){src++; curToken.len++;}}
	if (tokenCmpLiteral(curToken, "+")) { curToken.type = opToken; curToken.subtype = opAdd; }
	else if (tokenCmpLiteral(curToken, "-")) { curToken.type = opToken; curToken.subtype = opSub; }
	else if (tokenCmpLiteral(curToken, "*")) { curToken.type = opToken; curToken.subtype = opMul; }
	else if (tokenCmpLiteral(curToken, "%")) { curToken.type = opToken; curToken.subtype = opDiv; }          // division now %
	else if (tokenCmpLiteral(curToken, "ref")) { curToken.type = opToken; curToken.subtype = opReference; }    // / is reference
	else if (tokenCmpLiteral(curToken, "deref")) { curToken.type = opToken; curToken.subtype = opDereference; } // \ is dereference
	else if (tokenCmpLiteral(curToken, "=")) { curToken.type = opToken; curToken.subtype = opEqual; }
	else if (tokenCmpLiteral(curToken, ">")) { curToken.type = opToken; curToken.subtype = opCmpGreater; }
	else if (tokenCmpLiteral(curToken, "<")) { curToken.type = opToken; curToken.subtype = opCmpLess; }
	else if (tokenCmpLiteral(curToken, "==")) { curToken.type = opToken; curToken.subtype = opCmpEqual; }
	else if (tokenCmpLiteral(curToken, "|")) { curToken.type = opToken; curToken.subtype = opBitwiseOr; }
	else if (tokenCmpLiteral(curToken, "&")) { curToken.type = opToken; curToken.subtype = opBitwiseAnd; }
	else if (tokenCmpLiteral(curToken, "or")) { curToken.type = opToken; curToken.subtype = opLogicalOr; }
	else if (tokenCmpLiteral(curToken, "and")) { curToken.type = opToken; curToken.subtype = opLogicalAnd; }
	else if (tokenCmpLiteral(curToken, "!")) { curToken.type = opToken; curToken.subtype = opBitwiseNot; }
	else if (tokenCmpLiteral(curToken, "not")) { curToken.type = opToken; curToken.subtype = opLogicalNot; }
	else if (tokenCmpLiteral(curToken, "castptr")) { curToken.type = opToken; curToken.subtype = opPointerCast; }

	// clamps
	else if (tokenCmpLiteral(curToken, "(")) { curToken.type = clampToken; curToken.subtype = parenthesesL; }
	else if (tokenCmpLiteral(curToken, ")")) { curToken.type = clampToken; curToken.subtype = parenthesesR; }
	else if (tokenCmpLiteral(curToken, "{")) { curToken.type = clampToken; curToken.subtype = curlyBL; }
	else if (tokenCmpLiteral(curToken, "}")) { curToken.type = clampToken; curToken.subtype = curlyBR; }

	// end of line
	else if (tokenCmpLiteral(curToken, ";")) { curToken.type = endLine; }

	// size (byte)
	else if (tokenCmpLiteral(curToken, "word")) { curToken.type = sizeToken; curToken.subtype = 0; }

	// keywords
	else if (tokenCmpLiteral(curToken, "if")) { curToken.type = keywordToken; curToken.subtype = ifKey; }
	else if (tokenCmpLiteral(curToken, "for")) { curToken.type = keywordToken; curToken.subtype = forKey; }
	else if (tokenCmpLiteral(curToken, "while")) { curToken.type = keywordToken; curToken.subtype = whileKey; }
	else if (tokenCmpLiteral(curToken, "continue")) { curToken.type = keywordToken; curToken.subtype = continueKey; }
	else if (tokenCmpLiteral(curToken, "break")) { curToken.type = keywordToken; curToken.subtype = breakKey; }
	else if (tokenCmpLiteral(curToken, "global")) { curToken.type = keywordToken; curToken.subtype = globalKey; }

	else if (curToken.str[0] >= '0' && curToken.str[0] <= '9') { curToken.type = constantToken; curToken.subtype = 0; }

	// fallback on null
	else {
		curToken.type = identifierToken;
		curToken.subtype = 0;
	}
	printf("%.*s\n", curToken.len, curToken.str);
}

//#############################################################################################################################################
// HEX EMITTER

typedef enum specialRegs {
	scratchReg1 = 10,
	scratchReg2 = 11,
	scratchReg3 = 12,
	stackPointerReg = 13
};

typedef enum opcode {
	mov_reg_reg_32, movs, movw_lit_32, movt_lit_32,
	ldrw_imm_32, strw_imm_32, ldrw_reg_32, strw_reg_32,
	subw_imm_32, addw_imm_32, subw_reg_32, addw_reg_32
}opcode;

void emitOpcode(const uint8_t code) {
	switch (code) {
	case mov_reg_reg_32: printf("MOV_REG_REG_32 (reg)");    break;
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
	default:             printf("UNKNOWN_OP");   break;
	}
	printf("\n");
	fflush(stdout);
}

void emitFlag(const uint8_t flag) {
	printf(flag ? "CARRY\n" : "NO CARRY\n");
}

void emitArgument(const uint32_t arg, const uint8_t sz) {
	printf("%d\n", arg);
}

enum dirty_ { mod, unmod };

typedef struct {
	uint32_t stackOffset;
	uint16_t dirty, priority;
}registerData;

#define maxGPRegs 10
registerData virtualRegFile[maxGPRegs]; // 10 data registers

void initializeVirtualRegs() {
	for (uint8_t idx = 0; idx < maxGPRegs; idx++) {
		virtualRegFile[idx] = (registerData){ 0,0,0 };
	}
}

void storeConstantInReg(const uint8_t reg, const uint32_t c) {
	emitOpcode(movw_lit_32); emitArgument(reg, 4); emitArgument(c, 16);
	emitOpcode(movt_lit_32); emitArgument(reg, 4); emitArgument(c, 16);
}

void storeRegisterIntoStack(const uint8_t reg, const uint32_t stackOffset) {
	if (stackOffset >= 4095) {
		storeConstantInReg(scratchReg3, stackOffset);
		emitOpcode(strw_reg_32); emitArgument(stackPointerReg, 4); emitArgument(reg, 4);
		emitArgument(scratchReg3, 4);
		return;
	}
	emitOpcode(strw_imm_32); emitArgument(stackPointerReg, 4); emitArgument(reg, 4);
	emitArgument(stackOffset, 12);
}

void loadRegisterFromStack(const uint8_t reg, const uint32_t stackOffset) {
	if (stackOffset >= 4095) {
		emitOpcode(movw_lit_32); emitArgument(scratchReg3, 4); emitArgument(stackOffset, 16);
		emitOpcode(movt_lit_32); emitArgument(scratchReg3, 4); emitArgument(stackOffset, 16);
		emitOpcode(ldrw_reg_32); emitArgument(stackPointerReg, 4); emitArgument(reg, 4);
		emitArgument(scratchReg3, 4);
		return;
	}
	emitOpcode(ldrw_imm_32); emitArgument(stackPointerReg, 4); emitArgument(reg, 4);
	emitArgument(stackOffset, 12);
}

forceinline void loadRegisterFromStackR(const uint8_t reg, const uint8_t reg2) {
	emitOpcode(ldrw_reg_32); emitArgument(stackPointerReg, 4); emitArgument(reg, 4);
	emitArgument(reg2, 4);
}

void flushRegister(uint8_t regIdx) {
	if(virtualRegFile[regIdx].dirty) storeRegisterIntoStack(regIdx, virtualRegFile[regIdx].stackOffset);
	virtualRegFile[regIdx].dirty = 0; virtualRegFile[regIdx].stackOffset = 0; // free
}

forceinline void flushRegisters(){
	for(uint8_t idx = 0; idx < maxGPRegs; idx++) flushRegister(idx);
}

uint8_t moveToRegs(const uint32_t stackOffset, const uint16_t priority) {
	uint8_t lowestPriority = 0, fIdx = 0;
	for (uint8_t r = 0; r < maxGPRegs; r++) {
		if (virtualRegFile[r].stackOffset == stackOffset && stackOffset != 1) { return r; }
		if (virtualRegFile[r].priority < lowestPriority) { lowestPriority = virtualRegFile[r].priority; fIdx = r; }
	}
	flushRegister(fIdx);
	loadRegisterFromStack(fIdx, stackOffset);
	virtualRegFile[fIdx] = (registerData){ stackOffset, unmod, priority };
	return fIdx;
}

//#############################################################################################################################################
// STACK/REGISTER MANAGER

enum operandType { constant, stackVar, registerVar, stackTmp, nullVar };

typedef struct{
	uint32_t stackOffset, size;
	const char* name; 
	uint8_t strLen; uint8_t scope;
}variableMetadata;

typedef struct operand {
	union {
		unionType value; variableMetadata* variable; unionType stackOffset;
	}val;
	uint32_t size;
	// address 16 gp regs
	uint8_t operandType; uint8_t registerLocation;
	uint8_t registerPreference;
}operand;
#pragma pack(push, 1)
typedef struct{uint8_t subtype, precedence;}operator;
#pragma pack(pop)

#define maxUserVariables 256
variableMetadata variableBuffer[maxUserVariables]; uint8_t variableIdx = 0;
uint32_t curStackOffset = 0; uint8_t curScope = 0;
uint32_t curCompilerTempSz = 0;

variableMetadata* addVariable(const char* name, const uint8_t strLen, const uint32_t size){
	variableBuffer[variableIdx] = (variableMetadata){curStackOffset, size, name, strLen, curScope}; 
	curStackOffset += size; curStackOffset = (curStackOffset + 3) & ~3;
	return variableBuffer + (variableIdx++);
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

operand retrieveLocalVariable(const char* name, uint8_t len){
	for (variableMetadata* v = variableBuffer + variableIdx - 1; v >= variableBuffer; v--) {
		if (compareNames(name, len, v->name, v->strLen)) { return (operand) { .val.variable = v, v->size, stackVar, 0 }; }
	}
	return (operand) { 0, nullVar, 0 };
}

//#############################################################################################################################################
// OPERATION PARSER/HEX EMITTER

#define nullMove 1

operand assembleOp(const operator op, const operand* operands, const uint8_t registerPermanence){
	operand ret, o1, o2;
	switch(op.subtype){
		case opEqual:{
		o2 = *operands; o1 = *(operands - 1);
		uint32_t num32BitTransfers = ((o1.size < o2.size ? o1.size : o2.size) + 3) >> 2;
		ret = o1;
		do{
			num32BitTransfers--;
			uint8_t regDest;
			if(o1.operandType == registerVar){
				regDest = o1.registerLocation + num32BitTransfers;
				virtualRegFile[regDest].dirty = 1;
			} else regDest = scratchReg1;
			switch(o2.operandType){
				case registerVar:
				emitOpcode(mov_reg_reg_32); emitArgument(regDest, 4); emitArgument(o2.registerLocation + num32BitTransfers, 4);
				break;
				case stackVar:
				for(uint8_t ridx = 0; ridx < maxGPRegs; ridx++){
					if(virtualRegFile[ridx].stackOffset == o2.val.variable->stackOffset + num32BitTransfers * 4){
						virtualRegFile[ridx] = (registerData){0, 0, 0};
						break;
					}
				}
				loadRegisterFromStack(regDest, o2.val.variable->stackOffset + num32BitTransfers * 4);
				break;
				case stackTmp:
				loadRegisterFromStack(regDest, o2.val.stackOffset + num32BitTransfers * 4);
				for(uint8_t ridx = 0; ridx < maxGPRegs; ridx++){
					if(virtualRegFile[ridx].stackOffset == o2.val.stackOffset + num32BitTransfers * 4){
						virtualRegFile[ridx] = (registerData){0, 0, 0};
						break;
					}
				}
				break;
				case constant:
				storeConstantInReg(regDest, o2.val.value);
				break;
			}
			if(o1.operandType == registerVar) continue;
			storeRegisterIntoStack(regDest, o1.val.variable->stackOffset + num32BitTransfers * 4);
		}while(num32BitTransfers > 0);
		break;
		}
		case opAdd:{
		o2 = *operands; o1 = *(operands - 1);
		uint32_t num32BitTransfersG = ((o1.size > o2.size ? o1.size : o2.size) + 3) >> 2;
		uint8_t regDest; const uint32_t previousStackOffset = curStackOffset;
		curStackOffset += num32BitTransfersG * 4; curCompilerTempSz += num32BitTransfersG * 4;
		if(num32BitTransfersG > 1){
			regDest = scratchReg2;
			ret = (operand){previousStackOffset, num32BitTransfersG * 4, stackTmp, 0, registerPermanence};
		} else{
			regDest = moveToRegs(nullMove, registerPermanence); 
			virtualRegFile[regDest].stackOffset = previousStackOffset; virtualRegFile[regDest].dirty = 1;
			ret = (operand){previousStackOffset, 4, registerVar, regDest, registerPermanence};
		}
		uint8_t regOp1 = scratchReg1; uint8_t regOp2 = scratchReg2;
		for(uint32_t nt = 0; nt < num32BitTransfersG; nt++){
			switch(o1.operandType){
				case stackVar: loadRegisterFromStack(scratchReg1, o1.val.variable->stackOffset + nt * 4); break;
				case stackTmp: loadRegisterFromStack(scratchReg1, o1.val.stackOffset + nt * 4); break;
				case constant: storeConstantInReg(scratchReg1, o1.val.value * (nt == 0)); break;
				case registerVar: regOp1 = o1.registerLocation;
			}
			switch(o2.operandType){
				case stackVar: loadRegisterFromStack(scratchReg2, o2.val.variable->stackOffset + nt * 4); break;
				case stackTmp: loadRegisterFromStack(scratchReg2, o1.val.stackOffset + nt * 4); break;
				case registerVar: regOp2 = o2.registerLocation;
			}
			if (o2.operandType == constant && !nt) { 
				if (o2.val.value > 4095) {
					storeConstantInReg(scratchReg2, o2.val.value);
					emitOpcode(addw_reg_32); emitFlag(nt == 0); 
					emitArgument(regDest, 4); emitArgument(regOp1, 4); emitArgument(scratchReg2, 4);
				}
				else {
					emitOpcode(addw_imm_32); emitFlag(nt == 0); emitArgument(regDest, 4);
					emitArgument(regOp1, 4);/*carry or not*/ emitArgument(o2.val.value, 12);
				}
			}
			else {
				emitOpcode(addw_reg_32); emitFlag(nt == 0); emitArgument(regDest, 4); emitArgument(regOp1, 4); emitArgument(regOp2, 4);
			}
			if (num32BitTransfersG > 1) { storeRegisterIntoStack(scratchReg2, previousStackOffset + nt * 4); }
		}
		break;
		}
		case opDereference:{
		o2 = *operands; o1 = *(operands - 1);
		uint8_t regDest; const uint32_t previousStackOffset = curStackOffset;
		curStackOffset += o1.val.value * 4; curCompilerTempSz += o1.val.value * 4;
		if(o1.val.value > 1){
			regDest = scratchReg2;
			ret = (operand){previousStackOffset, o1.val.value * 4, stackTmp, 0, registerPermanence};
		} else{
			regDest = moveToRegs(nullMove, registerPermanence); 
			virtualRegFile[regDest].stackOffset = previousStackOffset;
			ret = (operand){previousStackOffset, 4, registerVar, regDest, registerPermanence};
		}
		for(uint32_t nt = 0; nt < o1.val.value; nt++){
			switch(o2.operandType){
				case registerVar:
				emitOpcode(mov_reg_reg_32); emitArgument(regDest, 4); emitArgument(o2.registerLocation, 4);
				break;
				case stackVar:
				loadRegisterFromStack(regDest, o2.val.variable->stackOffset + nt);
				break;
				case stackTmp:
				loadRegisterFromStack(regDest, o2.val.stackOffset + nt);
			}
			if(o1.val.value == 1){break;}
			storeRegisterIntoStack(regDest, previousStackOffset + nt * 4);
		}
		break;
		}
		case opReference:{
		o1 = *operands;
		if(o1.operandType == registerVar) flushRegister(o1.registerLocation);
		uint8_t regDest = moveToRegs(1, registerPermanence); virtualRegFile[regDest].stackOffset = curStackOffset;
		virtualRegFile[regDest].dirty = 1; storeConstantInReg(regDest, o1.val.stackOffset);
		curStackOffset += 4; curCompilerTempSz += 4;
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
			if(allocationSz > 0) operandStack[operandIdx++] = (operand){.val.variable = addVariable(curToken.str, curToken.len, allocationSz)};
			else operandStack[operandIdx++] = retrieveLocalVariable(curToken.str, curToken.len);
			break;
			case opToken:
			const uint32_t curPrecedence = operatorPrecedence[curToken.subtype] + precedence;
			uint32_t prevPrecedence = UINT32_MAX;
			while(curPrecedence <= prevPrecedence){
				prevPrecedence = operatorIdx > 0 ? operatorStack[operatorIdx-1].precedence : 0;
				const operand tmp = assembleOp(operatorStack[--operatorIdx], operandStack + operandIdx - 1, registerPermanence);
				operandIdx -= operatorStack[operatorIdx].subtype == opReference ? 1 : 2;
				operandStack[operandIdx++] = tmp;
			}
			else operatorStack[operatorIdx++] = (operator){curToken.subtype, curPrecedence};
			break;
			case clampToken:
			precedence += (curToken.subtype == parenthesesL) * 10 - (curToken.subtype == parenthesesR) * 10;
			break;
			case endLine:
			goto breakOutOfShunting;
		}
	}
	breakOutOfShunting:;
	for(int8_t idx = operatorIdx - 1; idx >= 0; idx--){
		const operand tmp = assembleOp(operatorStack[idx], operandStack + operandIdx - 1, registerPermanence);
		operandIdx -= operatorStack[idx].subtype == opReference ? 1 : 2;
		operandStack[operandIdx++] = tmp;
	}
}