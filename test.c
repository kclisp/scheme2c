#include <stdio.h>
#include <stdint.h>

//Object definition
union Object {
  uint64_t u;
  double d;
};

typedef union Object Object;

//nan tag components
#define exp_ones ((uint64_t)0x7FF << 52)
#define int_tag ((uint64_t)1<<51)
#define pointer_tag(x) ((uint64_t)x<<49)
#define pointer_mask pointer_tag(11)
#define cons_tag pointer_tag(00)
#define symbol_tag pointer_tag(01)
#define string_tag pointer_tag(10)
#define vector_tag pointer_tag(11)

//casts
Object integer_to_obj(uint64_t num) {
  return (Object)(num + exp_ones + int_tag);
}
Object double_to_obj(double num) {
  return (Object){.d = num};
}
    
//predicates
int has_flag(Object obj, long mask, long flag) {
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
int pointer_has_flag(Object obj, long flag) {
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

//type tests
void print_obj(Object obj) {
  printf("a: %p l: %lx d: %f    \t f? %d i? %d c? %d sy? %d st? %d v? %d\n",
         &obj, obj.u, obj.d, float_typep(obj), int_typep(obj),
         cons_typep(obj), symbol_typep(obj), string_typep(obj), vector_typep(obj));
}

void test_types() {
  Object obj = {.d = .5};
  print_obj(obj);
  obj = integer_to_obj(20);
  print_obj(obj);
  obj = double_to_obj(3.1416);
  print_obj(obj);
}

int main() {
  test_types();
  return 0;
}
