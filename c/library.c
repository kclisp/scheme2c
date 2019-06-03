#include "library.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <inttypes.h>
#include <time.h>
#include "cons.h"
#include "boolean.h"
#include "procedure.h"
#include "symbol.h"

//TODO: signal errors
//TODO: test all primitives
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
Object pairpl(Object argl) {
  return bool_to_obj(cons_typep(car(argl)));
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

static void print_cons(Object obj)  {
  printf("(");
  while (pairp(obj)) {
    displayi(car(obj));
    obj = cdr(obj);
    if (pairp(obj))
      printf(" ");
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
  printf("#[compiled procedure %p]", obj_clear(compiled_procedure_entry(obj)));
}
static void print_adr(Object obj)   {printf("%p", obj_clear(obj));}
static void print_bool(Object obj)  {if (falsep(obj)) printf("#f"); else printf("#t");}
static void print_int(Object obj)   {printf("%" PRIu64, obj_to_int(obj));}

typedef void (*printer)(Object);
static printer printers[] = {print_cons, print_sym, print_str, print_vec,
                             print_pproc, print_cproc, print_adr, print_bool, print_int};
  
//internal
void displayi(Object obj) {
  if (dbl_typep(obj)) {
    printf("%f", obj.d);
    return;
  }
  //jump table on tag
  printers[obj_tag(obj)](obj);
}

//currently, this value isn't used
#define END_OF_INPUT (Object)((uint64_t)-1 + exp_ones + bool_tag)
#define BUF_SIZE 1 << 7
//possible to move this to library?
Object read(Object argl) {
  //ignore argl for now; read from user
  char buf[BUF_SIZE];
  fgets(buf, BUF_SIZE, stdin);
  return readi(buf);            /* returns END_OF_INPUT if nothing */
}

Object readi(char *buf) {
  if (*buf == 0)
    return END_OF_INPUT;
  while (isspace(*buf))
    buf++;
  if (*buf == '(')
    return readi_list(buf + 1);
  if (*buf == '#') {
    buf++;
    if (*buf == 0)
      return END_OF_INPUT;
    if (*buf == 't')
      return true;
    if (*buf == 'f')
      return false;
    printf("Unknown rest of input: %s\n", buf); /* maybe print whole buffer */
    return END_OF_INPUT;
  }
  if (*buf == '\'')
    return cons(sym_to_obj("quote"), readi_list(buf + 1)); /* read list because it's consed */
  if (*buf == '"')
    return readi_string(buf);
  if (isalnum(*buf) || (*buf == '.'))
    return readi_atom(buf);
}

//open paren is skipped
Object readi_list(char *buf) {
  /* Object first; */
  /* Object rest = readi_list(); */
  /* return cons(first, rest); */
  //TODO
}
char *readi_to_string(char *buf) {
  assert(string_free_index + BUF_SIZE <= max_string_mem);
  int ret = sscanf(buf, "%s", string_mem + string_free_index);
  char *str = string_mem + string_free_index;
  string_free_index += ret;
  return str;
}
Object readi_string(char *buf) {
  return str_to_obj(readi_to_string(buf));
}
//checks that the characters from [start, end) all satisfy func
//if end is null, check unti NULL is reached
//could be generalized
//returns 0 if false, 1 if truex
static int strsats(char *start, char *end, int (*func)(int)) {
  if (end) {
    for (;start < end; start++) {
      if (!func(*start))
        return 0;
    }
  } else {
    while (*start != 0) {
      if (!func(*start))
        return 0;
    }
  }
  return 1;
}
Object readi_atom(char *buf) {
  //if all numbers, no dot -- integer
  //all numbers, but with 1 dot -- doubleNN
  //else symbol
  char *dot = strchr(buf, '.');
  if (dot) {
    if (strsats(buf, dot, isdigit) &&
        strsats(dot, NULL, isdigit)) {
      double dbl;
      sscanf(buf, "%f", &dbl);
      return dbl_to_obj(dbl);
    }
    return readi_symbol(buf);
  }
  if (strsats(buf, NULL, isdigit)) {
    int64_t i;
    sscanf(buf, PRId64, &i);
    return int_to_obj(i);
  }
  return readi_symbol(buf);
}
//read string, then cast to symbol
Object readi_symbol(char *buf) {
  return sym_to_obj(readi_to_string(buf));
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
