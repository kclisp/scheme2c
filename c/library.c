#include "library.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include "cons.h"
#include "boolean.h"
#include "io.h"

//TODO: signal errors
//predicates
//equality
extern int eqp(Object, Object); /* internal */

Object eqpl(Object argl) {
  return bool_to_obj(eqp(car(argl), car(cdr(argl))));
}
//types
Object numberpl(Object argl) {
  Object a = car(argl);
  return bool_to_obj(int_typep(a) || dbl_typep(a));
}
Object integerpl(Object argl) {
  return bool_to_obj(int_typep(car(argl)));
}
Object stringpl(Object argl) {
  return bool_to_obj(str_typep(car(argl)));
}
Object symbolpl(Object argl) {
  return bool_to_obj(sym_typep(car(argl)));
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

//comparator
Object numequal(Object argl) {
  Object a = car(argl);
  Object b = car(cdr(argl));
  assert(int_typep(a));
  assert(int_typep(b));
  return bool_to_obj(eqp(a, b));
}
Object less_than(Object argl) {
  Object a = car(argl);
  Object b = car(cdr(argl));
  int res;
  assert(truep(numberpl(a)) && truep(numberpl(b)));
  /* make dispatch on type */
  if (int_typep(a)) {
    if (int_typep(b))
      res = obj_to_int(a) < obj_to_int(b);
    else
      res = obj_to_int(a) < b.d;
  } else {
    if (int_typep(b))
      res = a.d < obj_to_int(b);
    else
      res = a.d < b.d;
  }
  return bool_to_obj(res);
}

//list
Object consl(Object argl) {
  return argl;
}
Object cons_star(Object argl) {
  if (nullp(cdr(argl)))
    return car(argl);
  return cons(car(argl), cons_star(cdr(argl)));
}
Object carl(Object argl) {
  return car(car(argl));
}
Object cdrl(Object argl) {
  return cdr(car(argl));
}
Object set_carml(Object argl) {
  set_carm(car(argl), car(cdr(argl)));
  // eventually unspecified
  return true;
}
Object set_cdrml(Object argl) {
  set_cdrm(car(argl), car(cdr(argl)));
  // eventually unspecified
  return true;
}
Object nullpl(Object argl) {
  return bool_to_obj(nullp(car(argl)));
}
Object pairpl(Object argl) {
  return bool_to_obj(pairp(car(argl)));
}

//output
Object newline(Object argl) {
  printf("\n");
}
Object display(Object argl) {
  displayi(car(argl));
  printf("\n");
  /* eventually. unspecified */
  return true;
}

//possible to move this to library?
Object read(Object argl) {
  //ignore argl for now; read from user
  char buf[READ_BUF_SIZE];
  fgets(buf, READ_BUF_SIZE, stdin);
  //get rid of newline
  int last = strlen(buf)-1;
  if (last > -1 && buf[last] == '\n')
    buf[last] = 0;
  
  return readi(buf);            /* returns END_OF_INPUT if nothing */
}

Object error(Object argl) {
  printf("ERROR: ");
  while (pairp(argl)) {
    displayi(argl);
  }
  printf("\n");
  /* eventually go to debugger maybe? */
  return false;
}

//time
Object clockl(Object argl) {
  return (Object)(uint64_t)clock();
}
