#include "symbol.h"
#include <string.h>
#include <assert.h>
#include "cons.h"
#include "boolean.h"

//obarray is an array of symbols. These symbols point to strings, which can be checked.
static Object obarray[obarray_max_symbols];
static int ob_free_index = 0;

//check if the str is str equal to any symbol in obarray
//return false if there is none
static Object obarray_sym(char *str) {
  int i;
  Object sym;
  for (i = 0; i < ob_free_index; i++) {
    sym = obarray[i];
    if (strcmp((char *)obj_clear(sym), str) == 0) /* strings are equal */
      return sym;
  }
  return false;
}

static void add_obarray_sym(Object sym) {
  assert(ob_free_index < obarray_max_symbols);
  obarray[ob_free_index++] = sym;
}

//need to check the obarray
//need immutability of str??
Object sym_to_obj(char *str) {
  Object sym = obarray_sym(str);
  if (truep(sym))
    return sym;
  sym = (Object)((uint64_t)str | exp_ones | sym_tag);
  add_obarray_sym(sym);
  return sym;
}
