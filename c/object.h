#ifndef OBJECT_H
#define OBJECT_H

#include <stdint.h>

//Object definition
union Object {
  uint64_t u;
  double d;
};

typedef union Object Object;

//nan tag components
#define exp_ones ((uint64_t)0x7FF << 52)
#define tag(x) ((uint64_t)(x)<<48)
#define pointer_mask tag(0b1111)
#define tag_start    tag(0b0001)
#define int_tag      tag(0b1000)
#define cons_tag     tag(0b0000)
#define symbol_tag   tag(0b0001)
#define string_tag   tag(0b0010)
#define vector_tag   tag(0b0011)
#define pproc_tag    tag(0b0100)
#define cproc_tag    tag(0b0101)
#define address_tag  tag(0b0110)

//predicates
int float_typep(Object);
int int_typep(Object);
int cons_typep(Object);
int symbol_typep(Object);
int string_typep(Object);
int vector_typep(Object);
int pproc_typep(Object);
int cproc_typep(Object);
int address_typep(Object);

//casts
Object integer_to_obj(uint64_t);
Object double_to_obj(double);
Object string_to_obj(char *);
Object symbol_to_obj(char *);
Object address_to_obj(char *);

//clear tags (not useful for float and int)
uint64_t obj_clear(Object);

#endif
