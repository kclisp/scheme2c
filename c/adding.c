#include <stdio.h>
#include <stdint.h>
#include "cons.h"
#include "environment.h"
#include "library.h"
#include "procedure.h"

int main() {
  int flag;
  char *cont, *entry;
  Object val, argl, proc;
  Env env = top_level_env();

proc = lookup_variable_value(sym_to_obj("+"), env);
val = int_to_obj(2);
argl = cons(val, nil);
val = int_to_obj(1);
argl = cons(val, argl);
flag = primitive_procedurep(proc);
if (flag) goto primitive_branch3;
compiled_branch2:
cont = &&after_call1;
entry = compiled_procedure_entry(proc);
goto *entry;
primitive_branch3:
val = apply_primitive_procedure(proc, argl);
after_call1:

  return 0;
}
