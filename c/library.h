#ifndef LIBRARY_H
#define LIBRARY_H

#include "object.h"

//predicates
//equality
int eqp(Object, Object);

//procedures
int primitive_procedurep(Object);
Object apply_primitive_procedure(Object, Object);
char *compiled_procedure_entry (Object);

#endif
