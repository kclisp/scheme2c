#include "environment.h"
#include <stddef.h>
#include <assert.h>
#include "cons.h"
#include "library.h"

//returns nil on failure
static Binding frame_get_binding(Object var, Frame frame) {
  Binding b;
  while (pairp(frame)) {
    b = car(frame);
    if (eqp(car(b), var))
      return b;
    frame = cdr(frame);
  }
  return nil;
}

//returns nil on failure
Binding env_get_binding(Object var, Env env) {
  Binding b;
  while (1) {
    b = frame_get_binding(var, env.frame);
    if (pairp(b))
      return b;
    Env *next = env.next_env;
    if (next == NULL)
      return nil;
    env = *next;
  }
}

//error if var is not in env
Object lookup_variable_value(Object var, Env env) {
  Binding b = env_get_binding(var, env);
  assert(pairp(b));
  return cdr(b);
}
