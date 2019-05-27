#include <stdio.h>
#include <stdint.h>
#include "object.h"
#include "cons.h"

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

int main() {
  test_types();

  int flag;
  char *cont, *entry;
  Object val, argl, proc, env;
  
  return 0;
}
