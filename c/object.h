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
#define sym_tag      tag(0b0001)
#define str_tag      tag(0b0010)
#define vector_tag   tag(0b0011)
#define pproc_tag    tag(0b0100)
#define cproc_tag    tag(0b0101)
#define adr_tag      tag(0b0110)
#define bool_tag     tag(0b0111)

//predicates
inline int has_flag(Object obj, long mask, long flag) {return (obj.u & mask) == flag;}

inline int dbl_typep(Object obj) {return !has_flag(obj, exp_ones, exp_ones);}

inline int tag_has_flag(Object obj, long flag) {
  return !dbl_typep(obj) && has_flag(obj, pointer_mask, flag);
}

int int_typep(Object);
inline int cons_typep(Object obj) {return tag_has_flag(obj, cons_tag);}
int sym_typep(Object);
int str_typep(Object);
int vector_typep(Object);
int pproc_typep(Object);
int cproc_typep(Object);
int adr_typep(Object);
int bool_typep(Object);

//casts
Object dbl_to_obj(double);
Object int_to_obj(int64_t);
Object str_to_obj(char *);
/* Object sym_to_obj(char *); in symbol.h*/
Object adr_to_obj(void *);
Object bool_to_obj(int);

int64_t obj_to_int(Object);
//clear tags (not useful for float and int)
inline uint64_t obj_clear(Object obj) {return obj.u & ~(exp_ones | pointer_mask);}

uint64_t obj_tag(Object);

void print_obj(Object);

#endif
