#include "io.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <inttypes.h>
#include "boolean.h"
#include "cons.h"
#include "symbol.h"
#include "procedure.h"

static void print_cons(Object obj)  {
  printf("(");
  while (pairp(obj)) {
    displayi(car(obj));
    obj = cdr(obj);
    if (pairp(obj))
      printf(" ");
    else if (!cons_typep(obj)) {
      printf(" . ");
      displayi(obj);
    }
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

typedef void (*printer)(Object);
static printer printers[] = {print_cons, print_sym, print_str, print_vec,
                             print_pproc, print_cproc, print_adr, print_bool};
  
//internal
void displayi(Object obj) {
  if (dbl_typep(obj)) {
    printf("%f", obj.d);
    return;
  }
  if (int_typep(obj)) {
    printf("%" PRIu64, obj_to_int(obj));
    return;
  }
  //jump table on tag
  printers[obj_tag(obj)](obj);
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
  assert(string_free_index + READ_BUF_SIZE <= max_string_mem);
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
    while (*start) {
      if (!func(*start))
        return 0;
      start++;
    }
  }
  return 1;
}
Object readi_atom(char *buf) {
  //if all numbers, no dot -- integer
  //all numbers, but with 1 dot -- double
  //else symbol
  char *dot = strchr(buf, '.');
  if (dot) {
    if (strsats(buf, dot, isdigit) &&
        strsats(dot+1, NULL, isdigit)) {
      double dbl;
      sscanf(buf, "%lf", &dbl);
      return dbl_to_obj(dbl);
    }
    return readi_symbol(buf);
  }
  if (strsats(buf, NULL, isdigit)) {
    int64_t i;
    sscanf(buf, "%" SCNd64, &i);
    return int_to_obj(i);
  }
  return readi_symbol(buf);
}
//read string, then cast to symbol
Object readi_symbol(char *buf) {
  return sym_to_obj(readi_to_string(buf));
}
