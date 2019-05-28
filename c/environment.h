#ifndef ENVIRONMENT_H
#define ENVIRONMENT_H

#include "object.h"

typedef Object Binding;         /* a binding is a cons: car is var, cdr is val */
typedef Object Frame;           /* a frame is a list (cons) of bindings */

struct Env {
  Frame frame;
  struct Env* next_env;
};

typedef struct Env Env;

Binding env_get_binding(Object, Env);
Object lookup_variable_value(Object, Env);

Env top_level_env();

#endif
