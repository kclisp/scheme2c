#ifndef STACK_H
#define STACK_H

#include "object.h"

#define stack_max_level 256
extern Object stack[];

void save(Object);
Object restore();

#endif
