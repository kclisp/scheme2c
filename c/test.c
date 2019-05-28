#include <stdio.h>
#include <stdint.h>
#include "object.h"
#include "cons.h"
#include "environment.h"
#include "library.h"

//type tests
void print_obj(Object obj) {
  printf("l: %lx d: %f      \tf? %d i? %d c? %d sy? %d st? %d v? %d\n",
         obj.u, obj.d, float_typep(obj), int_typep(obj),
         cons_typep(obj), symbol_typep(obj), string_typep(obj), vector_typep(obj));
}

void test_object() {
  printf("test_types\n");
  print_obj((Object){.d = .5});
  print_obj(integer_to_obj(20));
  print_obj(double_to_obj(3.1416));
  print_obj(cons(integer_to_obj(2), nil)); /* note: inf, instead of nan. desirable? */
  print_obj(nil);
  print_obj(symbol_to_obj("hi, friend"));
  char hi[] = "hi, friend";
  print_obj(symbol_to_obj(hi)); /* should be same */
  print_obj(string_to_obj("hello, world!"));
  print_obj(string_to_obj(hi)); /* should be different */
  //vector
}
void test_cons() {
  printf("test_cons\n");
  Object con = cons(cons(integer_to_obj(2), string_to_obj("hi")), nil);
  print_obj(con);
  print_obj(car(con));
  print_obj(cdr(con));
  printf("nullp(con) %d pairp(con) %d nullp(cdr(con)) %d\n",
         nullp(con), pairp(con), nullp(cdr(con)));
  print_obj(car(car(con)));
  print_obj(cdr(car(con)));
}
void test_env() {
  printf("test_env\n");
  Object x = symbol_to_obj("x");
  Object y = symbol_to_obj("y");
    
  Binding b = cons(x, integer_to_obj(3));
  Frame f = cons(b, nil);
  b = cons(y, integer_to_obj(40));
  f = cons(b, f);
  Env e = cons(f, nil);
  print_obj(env_get_binding(x, e));
  print_obj(env_get_binding(integer_to_obj(2), e));
  print_obj(lookup_variable_value(x, e));
  print_obj(lookup_variable_value(y, e));
}
void test_lib() {

}

void test() {
  test_object();
  test_cons();
  test_env();
}

int main() {
  test();

  int flag;
  char *cont, *entry;
  Object val, argl, proc;
  Env env = top_level_env();
  
  return 0;
}
