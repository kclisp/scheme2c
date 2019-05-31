#ifndef ENVIRONMENT_H
#define ENVIRONMENT_H

#include "object.h"

//Env is a pointer to env_mem. The first value is the parent env, (except for the
//top_level_env, which has 0 instead) and the rest are variable values.
typedef Object Env;

//Essentially a stack: variable values go here.
//Eventually free up memory too, when procedure is done.
#define max_env_mem_objects 1 << 21
extern Object env_mem[];
extern uint64_t env_free_index;

#define top_level_env (adr_to_obj(env_mem))

Object lexical_address_lookup(Object, Object, Env);

void define_variablem(Object, Object, Object, Env);

Object extend_environment(Object, Object, Env);
//eventually pass this data as metadata?
inline void increase_env_size(Object amount) {
  env_free_index += obj_to_int(amount);
}

#endif
