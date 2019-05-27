#ifndef OBJECT_H
#define OBJECT_H

#include <stdint.h>

//for pointers
extern char etext; //end of text segment

//Object definition
union Object {
  uint64_t u;
  double d;
};

typedef union Object Object;

//nan tag components
#define exp_ones ((uint64_t)0x7FF << 52)
#define int_tag ((uint64_t)1<<51)
#define pointer_tag(x) ((uint64_t)(x)<<49)
#define pointer_mask pointer_tag(0b11)
#define tag_start pointer_tag(0b01)
#define cons_tag pointer_tag(0b00)
#define symbol_tag pointer_tag(0b01)
#define string_tag pointer_tag(0b10)
#define vector_tag pointer_tag(0b11)

//predicates
int float_typep(Object);
int int_typep(Object);
int pointer_typep(Object);
int cons_typep(Object);
int symbol_typep(Object);
int string_typep(Object);
int vector_typep(Object);

//casts
Object integer_to_obj(uint64_t);
Object double_to_obj(double);
Object string_to_obj(char *);
Object symbol_to_obj(char *);

#endif
