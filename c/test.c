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
  print_obj(make_compiled_procedure(adr_to_obj(0), nil));
  print_obj(make_compiled_procedure(adr_to_obj(test_object), nil));
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

  Object con2 = con;
  con = cons(int_to_obj(41), con);
  displayi(con);
  displayi(con2);
  set_carm(cdr(con), str_to_obj("modified"));
  displayi(con);
  displayi(con2);
  printf("\n");
}
void test_env() {
  printf("test_env\n");
  Object add_proc = lexical_address_lookup(int_to_obj(0), int_to_obj(0), top_level_env);
  print_obj(add_proc);
  define_variablem(int_to_obj(0), int_to_obj(0), int_to_obj(2), top_level_env);
  print_obj(add_proc);
  printf("add: %p\n", add);
  
  Object env = extend_environment(int_to_obj(1), false, cons(int_to_obj(22), nil), top_level_env);
  print_obj(lexical_address_lookup(int_to_obj(0), int_to_obj(0), env));
  print_obj(lexical_address_lookup(int_to_obj(1), int_to_obj(0), env));

  print_obj(lexical_address_lookup(int_to_obj(0), int_to_obj(3), top_level_env));

  Object proc = lexical_address_lookup(int_to_obj(0), int_to_obj(8), top_level_env);
  print_obj(proc);
  apply_primitive_procedure(proc, cons(int_to_obj(30), cons(int_to_obj(40), nil)));
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

  argl = cons(int_to_obj(40), nil);
  print_obj(numberpl(argl));
  print_obj(integerpl(argl));
  print_obj(stringpl(argl));

  argl = cons(int_to_obj(40), argl);
  print_obj(numequal(argl));
  print_obj(less_than(argl));
  set_carm(argl, int_to_obj(39));
  print_obj(numequal(argl));
  print_obj(less_than(argl));
  
  displayi(cons(cons(int_to_obj(10), int_to_obj(2)), str_to_obj("hi")));
  printf("\n");

  /* printf("Please input: "); */
  /* Object obj = read(nil); */
  /* print_obj(obj); */
  /* displayi(obj); */
  /* printf("\n"); */

  displayi(argl);
  displayi(consl(argl));
}
void test_stack() {
  printf("test_stack\n");
  save(sym_to_obj("abc"));
  print_obj(restore());
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

  return 0;
}
