#ifndef ENVIRONMENT_H
#define ENVIRONMENT_H

#include "object.h"

//Env is a pointer to env_mem. The first value is the parent env, (except for the
//top_level_env, which has 0 instead) and the rest are variable values.
typedef Object Env;

//Essentially a stack: variable values go here.
//Eventually free up memory too, when procedure is done. -- failed
#define max_env_mem_objects 1 << 9 /* this causes memory to go up because of initialization */
extern Object env_mem[];
extern uint64_t env_free_index;

#define top_level_env (adr_to_obj(env_mem))

Object lexical_address_lookup(Object, Object, Env);

void define_variablem(Object, Object, Object, Env);
void set_variable_valuem(Object, Object, Object, Env);

Env extend_environment(Object, Object, Object, Env);
inline void retract_environment(Object amount) {
  env_free_index -= obj_to_int(amount);
}

//eventually pass this data as metadata?
inline void increase_env_size(Object amount) {
  env_free_index += obj_to_int(amount); /* only increase top-level-env */
}

#endif
