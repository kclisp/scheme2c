#include "cons.h"
#include <assert.h>
#include "library.h"

static Object new_cons() {
  assert(free_index < MAX_CONS);
  return (Object)(free_index++ + exp_ones + cons_tag);
};

Object cons(Object a, Object b) {
  the_cars[free_index] = a;
  the_cdrs[free_index] = b;
  return new_cons();
}

// optimization... can't be inlined with assert (?)
/* uint64_t cons_index(Object cons) { */
/*   assert(pairp(cons)); */
/*   return obj_clear(cons); */
/* } */

extern Object car(Object);
extern Object cdr(Object);

void set_carm(Object cons, Object val) {
  the_cars[cons_index(cons)] = val;
}
void set_cdrm(Object cons, Object val) {
  the_cdrs[cons_index(cons)] = val;
}

extern int nullp(Object);
extern int pairp(Object);
