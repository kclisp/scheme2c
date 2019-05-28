#ifndef PROCEDURE_H
#define PROCEDURE_H

#include "object.h"
#include "environment.h"

//procedures
//primitive procedure is just the function pointer
Object make_primitive_procedure(void *entry);
int primitive_procedurep(Object proc);
Object apply_primitive_procedure(Object proc, Object argl);

//compiled procedure is a cons of entry and env
Object make_compiled_procedure(char *entry, Env env);
char *compiled_procedure_entry (Object proc);
Env compiled_procedure_env(Object proc);


#endif
