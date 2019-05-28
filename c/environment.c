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
  while (pairp(env)) {
    b = frame_get_binding(var, car(env));
    if (pairp(b))
      return b;
    env = cdr(env);
  }
  return nil;
}

//error if var is not in env
Object lookup_variable_value(Object var, Env env) {
  Binding b = env_get_binding(var, env);
  assert(pairp(b));
  return cdr(b);
}

//put all primitives
Env top_level_env() {

}
