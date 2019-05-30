#include "cons.h"
#include <assert.h>
#include "library.h"

uint64_t free_index = 0;

static Object new_cons() {
  assert(free_index < MAX_CONS);
  return (Object)(free_index++ + exp_ones + cons_tag);
};

Object cons(Object a, Object b) {
  the_cars[free_index] = a;
  the_cdrs[free_index] = b;
  return new_cons();
}

static uint64_t cons_index(Object cons) {
  assert(cons_typep(cons));
  return obj_clear(cons);
}

Object car(Object cons) {
  assert(pairp(cons));
  return the_cars[cons_index(cons)];
}

Object cdr(Object cons) {
  assert(pairp(cons));
  return the_cdrs[cons_index(cons)];
}

void set_carm(Object cons, Object val) {
  the_cars[cons_index(cons)] = val;
}
void set_cdrm(Object cons, Object val) {
  the_cdrs[cons_index(cons)] = val;
}

extern int nullp(Object);
extern int pairp(Object);
