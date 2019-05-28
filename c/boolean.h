#ifndef BOOLEAN_H
#define BOOLEAN_H

#include "object.h"

#define true ((Object)((uint64_t)1 + exp_ones + bool_tag))
#define false ((Object)(exp_ones + bool_tag))

//internal c usage
int truep(Object);
int falsep(Object);

#endif
