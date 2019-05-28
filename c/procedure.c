#include "procedure.h"
#include "cons.h"

//procedures
//primitive procedure is just the function pointer

Object make_primitive_procedure(void *entry) {
  return (Object)((uint64_t)entry + exp_ones + pproc_tag);
}
int primitive_procedurep(Object proc) {
  return pproc_typep(proc);
}

typedef Object Objfun_ptr(Object);
Object apply_primitive_procedure(Object proc, Object argl) {
  return ((Objfun_ptr*) obj_clear(proc)) (argl);
}

//compiled procedure is a cons of entry and env
Object make_compiled_procedure(char *entry, Env env) {
  return cons(address_to_obj(entry), env);
}
char *compiled_procedure_entry(Object proc) {
  return (char *)obj_clear(car(proc));
}
Env compiled_procedure_env(Object proc) {
  return cdr(proc);
}

