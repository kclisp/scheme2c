#include "library.h"
#include <assert.h>
#include <stdio.h>
#include <inttypes.h>
#include <time.h>
#include "cons.h"
#include "boolean.h"
#include "procedure.h"

//predicates
//equality
//addresses are exactly equal
int eqp(Object a, Object b) {
  return a.u == b.u;
}

//arithmetic
Object add(Object argl) {
  int64_t sum = 0;
  while (pairp(argl)) {
    sum += obj_to_int(car(argl));
    argl = cdr(argl);
  }
  return int_to_obj(sum);
}
Object subtract(Object argl) {
  if (nullp(argl))
    return int_to_obj(0);
  int64_t base = obj_to_int(car(argl));
  argl = cdr(argl);
  return int_to_obj(base - obj_to_int(add(argl)));
}
Object multiply(Object argl) {
  if (nullp(argl))
    return int_to_obj(1);
  int64_t product = 1;
  while (pairp(argl)) {
    product *= obj_to_int(car(argl));
    argl = cdr(argl);
  }
  return int_to_obj(product);
}

Object numequal(Object argl) {
  Object a = car(argl);
  Object b = car(cdr(argl));
  assert(int_typep(a));
  assert(int_typep(b));
  return bool_to_obj(eqp(a, b));
}

//list - wrappers
Object consl(Object argl) {
  return argl;
}
Object carl(Object argl) {
  return car(car(argl));
}
Object cdrl(Object argl) {
  return cdr(car(argl));
}
Object nullpl(Object argl) {
  return bool_to_obj(nullp(car(argl)));
}

//output
Object display(Object argl) {
  displayi(car(argl));
  printf("\n");
  /* eventually. unspecified */
  return true;
}

static void print_cons(Object obj)  {
  printf("(");
  while (pairp(obj)) {
    displayi(car(obj));
    obj = cdr(obj);
    if (pairp(obj))
      printf(" ");
  }
  printf(")");
}
static void print_sym(Object obj)   {printf("%s", obj_clear(obj));}
static void print_str(Object obj)   {printf("%s", obj_clear(obj));}
static void print_vec(Object obj)   {printf("generic-vector");}
static void print_pproc(Object obj) {
  printf("#[primitive procedure %p]", obj_clear(obj));
}
static void print_cproc(Object obj) {
  printf("#[compiled procedure %p]", obj_clear(compiled_procedure_entry(obj)));
}
static void print_adr(Object obj)   {printf("%p", obj_clear(obj));}
static void print_bool(Object obj)  {if (falsep(obj)) printf("#f"); else printf("#t");}
static void print_int(Object obj)   {printf("%" PRIu64, obj_to_int(obj));}

typedef void (*printer)(Object);
static printer printers[] = {print_cons, print_sym, print_str, print_vec,
                             print_pproc, print_cproc, print_adr, print_bool, print_int};
  
//internal
void displayi(Object obj) {
  if (dbl_typep(obj)) {
    printf("%f", obj.d);
    return;
  }
  //jump table on tag
  printers[obj_tag(obj)](obj);
}

//time
Object clockl(Object argl) {
  return (Object)(uint64_t)clock();
}
