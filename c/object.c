#include "object.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

//predicates
extern int has_flag(Object, long, long);
extern int tag_has_flag(Object, long);

extern int dbl_typep(Object);

int int_typep(Object obj) {
  return !dbl_typep(obj) && has_flag(obj, int_tag, int_tag);
}
extern int cons_typep(Object);
int sym_typep(Object obj) {
  return tag_has_flag(obj, sym_tag);
}
int str_typep(Object obj) {
  return tag_has_flag(obj, str_tag);
}
int vector_typep(Object obj) {
  return tag_has_flag(obj, vector_tag);
}
int pproc_typep(Object obj) {
  return tag_has_flag(obj, pproc_tag);
}
int cproc_typep(Object obj) {
  return tag_has_flag(obj, cproc_tag);
}
int adr_typep(Object obj) {
  return tag_has_flag(obj, adr_tag);
}
int bool_typep(Object obj) {
  return tag_has_flag(obj, bool_tag);
}

//casts
Object dbl_to_obj(double num) {
  return (Object){.d = num};    /* check for Nan and inf? If so, map to unique Nan */
}
Object int_to_obj(int64_t num) {
  assert(labs(num) < int_tag);
  return (Object)(num | exp_ones | int_tag);
}
Object str_to_obj(char *str) {
  return (Object)((uint64_t)str | exp_ones | str_tag);
}    
Object adr_to_obj(void *address) {
  return (Object)((uint64_t)address | exp_ones | adr_tag);
}
Object bool_to_obj(int bool) {
  return (Object)(bool | exp_ones | bool_tag);
}

int64_t obj_to_int(Object obj) {
  if ((int64_t)obj.u < 0)
    return obj.u;
  return obj.u & ~(exp_ones | int_tag);
}
//clear tags (not useful for float and int)
extern uint64_t obj_clear(Object);

uint64_t obj_tag(Object obj) {
  return (obj.u >> 48) & 0b1111;
}

void print_obj(Object obj) {
  printf("l: %lx d: %f      \tf? %d i? %d c? %d sy? %d st? %d v? %d pp? %d cp? %d ad? %d b? %d\n",
         obj.u, obj.d, dbl_typep(obj), int_typep(obj),
         cons_typep(obj), sym_typep(obj), str_typep(obj), vector_typep(obj),
         pproc_typep(obj), cproc_typep(obj), adr_typep(obj), bool_typep(obj));
}
