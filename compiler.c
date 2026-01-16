#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include "compiler.h"

#if defined(__GNUC__) || defined(__clang__)
	#define forceinline static inline __attribute__(always_inline)
#else
	#define forceinline static inline
#endif

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

enum operandType { constant, stackVar, registerVar, globalVar, nullVar };

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

const char* src; token curToken;
void setSource(const char* c){src = c;}

void nextToken(){
	while(*src == ' ' || *src == '\n') src++;
	curToken = (token){src, 0, 0, 0};
	if(*src == '\0') return;
	curToken.len++;
	while(!isSingle(*src) && *src != ' ' && *src != '\n') {src++; curToken.len++;}
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

enum operandType { constant, stackVar, registerVar, globalVar, nullVar };

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

registerData virtualRegFile[10]; // 10 data registers

void initializeVirtualRegs() {
	for (uint8_t idx = 0; idx < 10; idx++) {
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
	emitArgument(virtualRegFile[reg].stackOffset, 12);
}

void flushRegister(uint8_t regIdx) {
	if (virtualRegFile[regIdx].dirty) { storeRegisterIntoStack(regIdx, virtualRegFile[regIdx].stackOffset); }
	virtualRegFile[regIdx].dirty = 0; virtualRegFile[regIdx].stackOffset = 0; // free
}

uint8_t moveToRegs(const uint32_t stackOffset, const uint16_t priority) {
	uint8_t lowestPriority = 0, fIdx = 0;
	for (uint8_t r = 0; r < 10; r++) {
		if (virtualRegFile[r].stackOffset == stackOffset && stackOffset != 1) { return r; }
		if (virtualRegFile[r].priority > lowestPriority) { lowestPriority = virtualRegFile[r].priority; fIdx = r; }
	}
	flushRegister(fIdx);
	loadRegisterFromStack(fIdx, stackOffset);
	virtualRegFile[fIdx] = (registerData){ stackOffset, unmod, priority };
	return fIdx;
}

//#############################################################################################################################################
// STACK/REGISTER MANAGER

typedef struct{
	uint32_t stackOffset, size;
	const char* name; uint8_t strLen; uint8_t scope;
}variableMetadata;

#define maxUserVariables 256
variableMetadata variableBuffer[maxUserVariables]; uint8_t variableIdx = 0;
uint32_t curStackOffset = 0; uint8_t curScope = 0;

void addVariable(const char* name, const uint32_t size){
	variableBuffer[variableIdx++] = (variableMetadata){curStackOffset, size, name, curScope}; 
	curStackOffset += size; curStackoffset = (curStackOffset + 3) & ~3;
}

void decrementScope(){
	curScope--; if (variableIdx == 0) { return; }
	for (variableMetadata* v = variableBuffer + variableIdx - 1; v >= variableBuffer && v->scope > curScope; v--) {
		variableIdx--;
	}
}
forceinline void incrementScope(){curScope++;}


