#include "object.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

//predicates
static int has_flag(Object obj, long mask, long flag) {
  return (obj.u & mask) == flag;
}

int dbl_typep(Object obj) {
  return !has_flag(obj, exp_ones, exp_ones);
}
int int_typep(Object obj) {
  return !dbl_typep(obj) && has_flag(obj, int_tag, int_tag);
}
static int tag_has_flag(Object obj, long flag) {
  return !dbl_typep(obj) && has_flag(obj, pointer_mask, flag);
}
int cons_typep(Object obj) {
  return tag_has_flag(obj, cons_tag);
}
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
Object int_to_obj(int64_t num) {
  assert(labs(num) < int_tag);
  return (Object)(num | exp_ones | int_tag);
}
Object dbl_to_obj(double num) {
  return (Object){.d = num};
}
Object str_to_obj(char *str) {
  return (Object)((uint64_t)str | exp_ones | str_tag);
}    
Object adr_to_obj(void *address) {
  return (Object)((uint64_t)address | exp_ones | adr_tag);
}

int64_t obj_to_int(Object obj) {
  if ((int64_t)obj.u < 0)
    return obj.u;
  return obj.u & ~(exp_ones | int_tag);
}
//clear tags (not useful for float and int)
uint64_t obj_clear(Object obj) {
  return obj.u & ~(exp_ones | pointer_mask);
}

uint64_t obj_tag(Object obj) {
  return (obj.u >> 48) & 0b1111;
}

void print_obj(Object obj) {
  printf("l: %lx d: %f      \tf? %d i? %d c? %d sy? %d st? %d v? %d pp? %d cp? %d ad? %d b? %d\n",
         obj.u, obj.d, dbl_typep(obj), int_typep(obj),
         cons_typep(obj), sym_typep(obj), str_typep(obj), vector_typep(obj),
         pproc_typep(obj), cproc_typep(obj), adr_typep(obj), bool_typep(obj));
}
