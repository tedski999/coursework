#ifndef BC_INFIX_H
#define BC_INFIX_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "stack.h"
#include "postfix.h"

double evaluate_infix_expression(char **expr, int nterms);

#endif
