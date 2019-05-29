#include <stdio.h>
#include <stdint.h>
#include "cons.h"
#include "environment.h"
#include "library.h"
#include "symbol.h"
#include "boolean.h"
#include "procedure.h"
#include "stack.h"

//type tests
void test_object() {
  printf("test_types\n");
  print_obj((Object){.d = .5});
  print_obj(dbl_to_obj(3.1416));
  print_obj(int_to_obj(20));
  print_obj(int_to_obj(-30));
  /* print_obj(int_to_obj((int64_t)1<<52)); */ //correct; int too large
  print_obj(cons(int_to_obj(2), nil)); /* note: inf, instead of nan. desirable? */
  print_obj(nil);
  print_obj(sym_to_obj("hi, friend"));
  char hi[] = "hi, friend";
  print_obj(sym_to_obj(hi)); /* should be same */
  print_obj(str_to_obj("hello, world!"));
  print_obj(str_to_obj(hi)); /* should be different */
  //vector
  print_obj(make_primitive_procedure(test_object));
  print_obj(make_primitive_procedure(add));
  print_obj(make_compiled_procedure(0, nil));
  print_obj(make_compiled_procedure(test_object, nil));
  print_obj(adr_to_obj(0));
  print_obj(adr_to_obj(test_object));
  print_obj(true);
  print_obj(false);
}
void test_cons() {
  printf("test_cons\n");
  Object con = cons(cons(int_to_obj(2), str_to_obj("hi")), nil);
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
  Object x = sym_to_obj("x");
  Object y = sym_to_obj("y");
    
  Binding b = cons(x, int_to_obj(3));
  Frame f = cons(b, nil);
  b = cons(y, int_to_obj(40));
  f = cons(b, f);
  Env e = cons(f, nil);
  print_obj(env_get_binding(x, e));
  print_obj(env_get_binding(int_to_obj(2), e));
  print_obj(lookup_variable_value(x, e));
  print_obj(lookup_variable_value(y, e));

  Object z = sym_to_obj("z");
  define_variablem(z, dbl_to_obj(3.45), e);
  print_obj(env_get_binding(z, e));
  print_obj(lookup_variable_value(z, e));
}
void test_proc() {
  printf("test_proc\n");
  Object proc = make_primitive_procedure(add);
  printf("pproc? %d\n", primitive_procedurep(proc));
  Object argl = cons(int_to_obj(10), cons(int_to_obj(13), nil));
  print_obj(apply_primitive_procedure(proc, argl));
  argl = cons(int_to_obj(-5), cons(int_to_obj(-2), nil));
  Object res = apply_primitive_procedure(proc, argl);
  print_obj(res);
  printf("res: %d\n", obj_to_int(res));
}
void test_lib() {
  printf("test_lib\n");
  Object argl = cons(int_to_obj(20), cons(int_to_obj(57), nil));
  print_obj(add(argl));
  argl = cons(int_to_obj(41), argl);
  print_obj(add(argl));
}
void test_stack() {

}

void test() {
  test_object();
  test_cons();
  test_env();
  test_proc();
  test_lib();
  test_stack();
}

int main() {
  test();

  int flag;
  char *cont, *entry;
  Object val, argl, proc;
  Env env = top_level_env();

  return 0;
}
