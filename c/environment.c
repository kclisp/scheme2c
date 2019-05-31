#include "environment.h"
#include "cons.h"
#include "library.h"
#include "procedure.h"

//put all primitives
Object env_mem[max_env_mem_objects] = {0,
                                      /* make_primitive_procedure - maybe should be a macro? */
#define PRIMITIVE(scheme_name, c_name) ((Object)((uint64_t)c_name + exp_ones + pproc_tag)),
#include "primitives.def"
#undef PRIMITIVE
                                       0};
  
//count up number of primitives
uint64_t env_free_index = 1
#define PRIMITIVE(scheme_name, c_name) + 1
#include "primitives.def"
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

Object extend_environment(Object num_vars, Object argl, Env env) {
  Object new_env = adr_to_obj(env_mem + env_free_index);
  int num;
  env_mem[env_free_index++] = env;
  for (num = obj_to_int(num_vars); num > 0; num--) {
    env_mem[env_free_index++] = car(argl);
    argl = cdr(argl);
  }
  //last variable -- bound to rest - TODO
  return new_env;
}

extern void increase_env_size(Object);
