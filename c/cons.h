#ifndef CONS_H
#define CONS_H

#include "object.h"

#define MAX_CONS 256
Object the_cars[MAX_CONS];
Object the_cdrs[MAX_CONS];
extern uint64_t free_index;

extern Object nil;

Object cons(Object, Object);

Object car(Object);
Object cdr(Object);

void set_carm(Object, Object);
void set_cdrm(Object, Object);

int nullp(Object);
int pairp(Object);

#endif
