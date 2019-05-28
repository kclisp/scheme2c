#include "environment.h"
#include <stddef.h>
#include <assert.h>
#include "cons.h"
#include "library.h"
#include "procedure.h"
#include "symbol.h"
#include "boolean.h"

//returns false on failure
static Binding frame_get_binding(Object var, Frame frame) {
  Binding b;
  while (pairp(frame)) {
    b = car(frame);
    if (nullp(b))
      return false;
    if (eqp(car(b), var))
      return b;
    frame = cdr(frame);
  }
}

static void add_binding_to_framem(Object var, Object val, Frame frame) {
  if (nullp(car(frame)))
    set_carm(frame, cons(var, val));
  else {
    set_cdrm(frame, cons(car(frame), cdr(frame)));
    set_carm(frame, cons(var, val));
  }
}

//returns false on failure
Binding env_get_binding(Object var, Env env) {
  Binding b;
  while (pairp(env)) {
    b = frame_get_binding(var, car(env));
    if (pairp(b))
      return b;
    env = cdr(env);
  }
  return false;
}

//error if var is not in env
Object lookup_variable_value(Object var, Env env) {
  Binding b = env_get_binding(var, env);
  assert(truep(b));
  return cdr(b);
}

void define_variablem(Object var, Object val, Env env) {
  Binding b = frame_get_binding(var, car(env));
  if (truep(b))
    set_cdrm(b, val);
  else
    add_binding_to_framem(var, val, car(env));
}

//put all primitives
Env top_level_env() {
  Env env = cons(cons(nil, nil), nil);
  define_variablem(sym_to_obj("+"), make_primitive_procedure(&add), env);
  return env;
}
