#include "cons.h"
#include <assert.h>
#include "library.h"

uint64_t free_index = 0;

static Object new_cons() {
  assert(free_index < MAX_CONS);
  return (Object)(free_index++ + exp_ones + cons_tag);
}

//nil has the last possible index
Object nil = (Object)((tag_start - 1) + exp_ones + cons_tag);

Object cons(Object a, Object b) {
  the_cars[free_index] = a;
  the_cdrs[free_index] = b;
  return new_cons();
}

static uint64_t cons_index(Object cons) {
  assert(cons_typep(cons));
  return cons.u - exp_ones - cons_tag;
}

Object car(Object cons) {
  assert(!nullp(cons));
  return the_cars[cons_index(cons)];
}

Object cdr(Object cons) {
  assert(!nullp(cons));
  return the_cdrs[cons_index(cons)];
}


int nullp(Object a) {
  return eqp(a, nil);
}
int pairp(Object a) {
  return cons_typep(a) && !nullp(a);
}
