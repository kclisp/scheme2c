#include "library.h"
#include "cons.h"

//predicates
//equality
int eqp(Object a, Object b) {
  return a.u == b.u;
}

//procedures
int primitive_procedurep(Object proc) {

}

Object apply_primitive_procedure(Object proc, Object argl) {

}

char *compiled_procedure_entry (Object proc) {

}

