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
int pointer_typep(Object obj) {
  return !float_typep(obj) && !int_typep(obj);
}
static int pointer_has_flag(Object obj, long flag) {
  return pointer_typep(obj) && has_flag(obj, pointer_mask, flag);
}
int cons_typep(Object obj) {
  return pointer_has_flag(obj, cons_tag);
}
int symbol_typep(Object obj) {
  return pointer_has_flag(obj, symbol_tag);
}
int string_typep(Object obj) {
  return pointer_has_flag(obj, string_tag);
}
int vector_typep(Object obj) {
  return pointer_has_flag(obj, vector_tag);
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
