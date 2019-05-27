#include <stdio.h>
#include <stdint.h>
#include <assert.h>

//for pointers
extern char etext, edata, end;

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

//casts
Object integer_to_obj(uint64_t num) {
  return (Object)(num + exp_ones + int_tag);
}
Object double_to_obj(double num) {
  return (Object){.d = num};
}
//pointers are displacements from a base pointer
//sanity check: the displacement should be less than the tags
Object string_to_obj(char *str) {
  /* printf("str: %p &etext: %p\n", str, &etext); */
  assert((uint64_t)(str - &etext) < tag_start);
  return (Object)((uint64_t)(str - &etext) + exp_ones + string_tag);
}    
//need to check the obarray...
Object symbol_to_obj(char *str) {
  assert((uint64_t)(str - &etext) < tag_start);
  return (Object)((uint64_t)(str - &etext) + exp_ones + symbol_tag);
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
  printf("a: %p l: %lx d: %f    \t f? %d i? %d p? %d c? %d sy? %d st? %d v? %d\n",
         &obj, obj.u, obj.d, float_typep(obj), int_typep(obj), pointer_typep(obj),
         cons_typep(obj), symbol_typep(obj), string_typep(obj), vector_typep(obj));
}

void test_types() {
  Object obj = {.d = .5};
  print_obj(obj);
  obj = integer_to_obj(20);
  print_obj(obj);
  obj = double_to_obj(3.1416);
  print_obj(obj);
  //cons
  //symbol -- obarray
  obj = symbol_to_obj("hi, friend");
  print_obj(obj);
  obj = string_to_obj("hello, world!");
  print_obj(obj);
  //vector
}

//base functions


int main() {
  printf("First address beyond:\n");
  printf("    program text segment(etext)      %10p\n", &etext);
  printf("    initialized data segment(edata)  %10p\n", &edata);
  printf("    uninitialized data segment (end) %10p\n", &end);

  test_types();

  int flag;
  char *cont, *entry;
  Object val, argl, proc, env;
  
  return 0;
}
