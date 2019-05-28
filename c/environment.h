#ifndef ENVIRONMENT_H
#define ENVIRONMENT_H

#include "object.h"

typedef Object Binding;         /* a binding is a cons: car is var, cdr is val */
typedef Object Frame;           /* a frame is a list (cons) of bindings */
typedef Object Env;             /* an env is a cons of frame and next env */

Binding env_get_binding(Object, Env);
Object lookup_variable_value(Object, Env);

void define_variablem(Object, Object, Env);
Env top_level_env();

#endif
