#include <stdint.h>

typedef enum{
	noError,
	unexpectedExpression,
	delimiterMismatch,
	undefinedVariable,
	operatorDepthExceeded,
	branchDepthExceeded,
	userVarLimitExceeded
}errorCodes;

uint8_t assembleSource(const char* src, uint8_t* progOrigin);