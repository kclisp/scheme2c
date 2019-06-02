#include "environment.h"
#include <assert.h>
#include "cons.h"
#include "library.h"
#include "procedure.h"
#include "boolean.h"

//put all primitives
Object env_mem[max_env_mem_objects] = {0,
                                      /* make_primitive_procedure - maybe should be a macro? */
#define PRIMITIVE(scheme_name, c_name) ((Object)((uint64_t)c_name + exp_ones + pproc_tag)),
#include "../data/primitives.def"
#undef PRIMITIVE
                                       0};
  
//count up number of primitives
uint64_t env_free_index = 1
#define PRIMITIVE(scheme_name, c_name) + 1
#include "../data/primitives.def"
#undef PRIMITIVE
                          ;


static Object *lexical_address(int env_num, int offset_num, Env env) {
  for (; env_num > 0; env_num--)
    env = *(Object *)obj_clear(env);
  return (Object *)obj_clear(env) + offset_num + 1; /* skip parent env */
}

Object lexical_address_lookup(Object env_num, Object offset_num, Env env) {
  return *lexical_address(obj_to_int(env_num), obj_to_int(offset_num), env);
}

//should be the same as assignment, once i get to it
void define_variablem(Object env_num, Object offset_num, Object val, Env env) {
  *lexical_address(obj_to_int(env_num), obj_to_int(offset_num), env) = val;
}

Env extend_environment(Object num_vars, Object rest, Object argl, Env env) {
  Env new_env = adr_to_obj(env_mem + env_free_index);
  int num = obj_to_int(num_vars);
  assert(env_free_index + num <= max_env_mem_objects);
  
  env_mem[env_free_index++] = env; /* put parent env */
  for (; num > 1; num--) {
    env_mem[env_free_index++] = car(argl);
    argl = cdr(argl);
  }
  if (falsep(rest))
    env_mem[env_free_index++] = car(argl);
  else
    env_mem[env_free_index++] = argl; /* rest arg */

  return new_env;
}
extern void retract_environment(Object);

extern void increase_env_size(Object);

