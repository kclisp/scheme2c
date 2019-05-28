#include "object.h"
#include <assert.h>
#include <stdio.h>

//predicates
static int has_flag(Object obj, long mask, long flag) {
  return (obj.u & mask) == flag;
}

int float_typep(Object obj) {
  return !has_flag(obj, exp_ones, exp_ones);
}
int int_typep(Object obj) {
  return !float_typep(obj) && has_flag(obj, int_tag, int_tag);
}
static int tag_has_flag(Object obj, long flag) {
  return !float_typep(obj) && has_flag(obj, pointer_mask, flag);
}
int cons_typep(Object obj) {
  return tag_has_flag(obj, cons_tag);
}
int symbol_typep(Object obj) {
  return tag_has_flag(obj, symbol_tag);
}
int string_typep(Object obj) {
  return tag_has_flag(obj, string_tag);
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
int address_typep(Object obj) {
  return tag_has_flag(obj, address_tag);
}

//casts
Object integer_to_obj(uint64_t num) {
  return (Object)(num + exp_ones + int_tag);
}
Object double_to_obj(double num) {
  return (Object){.d = num};
}
Object string_to_obj(char *str) {
  return (Object)((uint64_t)str + exp_ones + string_tag);
}    
//need to check the obarray...
Object symbol_to_obj(char *str) {
  return (Object)((uint64_t)str + exp_ones + symbol_tag);
}
Object address_to_obj(char *address) {
  return (Object)((uint64_t)address + exp_ones + address_tag);
}

//clear tags
uint64_t obj_clear(Object obj) {
  return obj.u & ~(exp_ones | pointer_mask);
}
