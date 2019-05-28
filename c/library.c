#include "library.h"
#include "cons.h"

//predicates
//equality
int eqp(Object a, Object b) {
  return a.u == b.u;
}

