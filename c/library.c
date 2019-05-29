#include "library.h"
#include <assert.h>
#include <stdio.h>
#include <inttypes.h>
#include "cons.h"
#include "boolean.h"
#include "procedure.h"

//predicates
//equality
int eqp(Object a, Object b) {
  return a.u == b.u;
}

//arithmetic
Object add(Object argl) {
  Object a = car(argl);
  Object b = car(cdr(argl));
  assert(int_typep(a));
  assert(int_typep(b));
  return int_to_obj(obj_to_int(a) + obj_to_int(b));
}

Object display(Object argl) {
  Object a = car(argl);
  displayi(a);
  printf("\n");
  /* eventually. unspecified */
  return true;
}

typedef void (*printer)(Object);
static void print_cons(Object obj)  {
  printf("(");
  Object a;
  while (pairp(obj)) {
    a = car(obj);
    displayi(a);
    printf(" ");
    obj = cdr(obj);
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
  printf("#[compiled procedure %p]", compiled_procedure_entry(obj));
}
static void print_adr(Object obj)   {printf("%p", obj_clear(obj));}
static void print_bool(Object obj)  {if (falsep(obj)) printf("#f"); else printf("#t");}
static void print_int(Object obj)   {printf("%" PRIu64, obj_to_int(obj));}

static printer printers[] = {print_cons, print_sym, print_str, print_vec,
                             print_pproc, print_cproc, print_adr, print_bool, print_int};
  
//internal
void displayi(Object obj) {
  if (dbl_typep(obj)) {
    printf("%f", obj);
    return;
  }
  //jump table on tag
  printers[obj_tag(obj)](obj);
}
