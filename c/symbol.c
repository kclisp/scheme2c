#include "symbol.h"
#include <string.h>
#include "cons.h"
#include "boolean.h"

//obarray is a list of symbols. These symbols point to strings, which can be checked.
static Object obarray = nil;

//check if the str is str equal to any symbol in obarray
//return false if there is none
static Object obarray_sym(char *str) {
  Object arr;
  Object sym;
  for (arr = obarray; pairp(arr); arr = cdr(arr)) {
    sym = car(arr);
    if (strcmp((char *)obj_clear(sym), str) == 0) /* strings are equal */
      return sym;
  }
  return false;
}

static void add_obarray_symm(Object sym) {
  obarray = cons(sym, obarray);
}

//need to check the obarray
//need immutability of str??
Object sym_to_obj(char *str) {
  Object sym = obarray_sym(str);
  if (truep(sym))
    return sym;
  sym = (Object)((uint64_t)str | exp_ones | sym_tag);
  add_obarray_symm(sym);
  return sym;
}
