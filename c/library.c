#include "library.h"
#include <assert.h>
#include "cons.h"

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
