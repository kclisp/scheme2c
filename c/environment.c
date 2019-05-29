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
  return false;
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
    set_carm(env, cons(cons(var, val), car(env)));
}

Object extend_environment(Object vars, Object argl, Env env) {
  Object var, val;
  Frame f = nil;
  while (pairp(vars)) {
    var = car(vars);
    val = car(argl);
    f = cons(cons(var, val), f);
    vars = cdr(vars);
    argl = cdr(argl);
  }
  if (!nullp(vars))             /* vars has rest arg */
    f = cons(cons(vars, argl), f);
  return cons(f, env);
}

//top level should be a constant
//put all primitives
static void define_primitive(char *sym, void *fn, Env env) {
  define_variablem(sym_to_obj(sym), make_primitive_procedure(fn), env);
}
Env top_level_env() {
  Env env = cons(nil, nil);
  //arithmetic
  define_primitive("+", add, env);
  define_primitive("-", subtract, env);
  define_primitive("*", multiply, env);
  define_primitive("=", numequal, env);

  //list
  define_primitive("cons", consl, env);
  define_primitive("car", carl, env);
  define_primitive("cdr", cdrl, env);
  define_primitive("null?", nullpl, env);
  
  //output
  define_primitive("display", display, env);

  //time
  define_primitive("clock", clockl, env);
  
  return env;
}
