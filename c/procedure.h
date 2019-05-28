#ifndef PROCEDURE_H
#define PROCEDURE_H

#include "object.h"

//procedures
int primitive_procedurep(Object proc);

Object apply_primitive_procedure(Object proc, Object argl);

char *compiled_procedure_entry (Object proc);

#endif
