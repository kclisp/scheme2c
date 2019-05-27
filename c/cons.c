#include "cons.h"
#include <assert.h>

uint64_t free_index = 0;

Object new_cons() {
  assert(free_index < MAX_CONS);
  return (Object)(free_index++ + exp_ones + cons_tag);
}

Object cons(Object a, Object b) {
  the_cars[free_index] = a;
  the_cdrs[free_index] = b;
  return new_cons();
}
