#ifndef CONS_H
#define CONS_H

#include "object.h"

#define MAX_CONS 256
Object the_cars[MAX_CONS];
Object the_cdrs[MAX_CONS];
extern uint64_t free_index;

Object new_cons();
Object cons(Object, Object);

#endif
