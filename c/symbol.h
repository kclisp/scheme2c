#ifndef SYMBOL_H
#define SYMBOL_H

#include "object.h"

#define obarray_max_symbols 1<<9

Object sym_to_obj(char *);

#endif
