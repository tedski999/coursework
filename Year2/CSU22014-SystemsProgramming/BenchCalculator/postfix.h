#ifndef BC_POSTFIX_H
#define BC_POSTFIX_H_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <math.h>
#include "stack.h"

double evaluate_postfix_expression(char **expr, int nterms);

#endif
