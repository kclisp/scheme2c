#ifndef CONS_H
#define CONS_H

#include "object.h"
#include "library.h"

#define MAX_CONS 1<<24
Object the_cars[MAX_CONS];
Object the_cdrs[MAX_CONS];
uint64_t free_index;

//nil has the last possible index
#define nil ((Object)((tag_start - 1) + exp_ones + cons_tag))

Object cons(Object, Object);

inline uint64_t cons_index(Object cons) {return obj_clear(cons);}
  
inline Object car(Object cons) {return the_cars[cons_index(cons)];}
inline Object cdr(Object cons) {return the_cdrs[cons_index(cons)];}

void set_carm(Object, Object);
void set_cdrm(Object, Object);

inline int nullp(Object a) {return eqp(a, nil);}
inline int pairp(Object a) {return cons_typep(a) && !nullp(a);}

#endif
