#ifndef LIBRARY_H
#define LIBRARY_H

#include "object.h"

//addresses are exactly equal
inline int eqp(Object a, Object b) {return a.u == b.u;}

#define PRIMITIVE(scheme_name, c_name) Object c_name(Object);
#include "../data/primitives.def"
#undef PRIMITIVE

void displayi(Object);
Object readi(char *);
Object readi_list(char *);

#define max_string_mem 1 << 11
char string_mem[max_string_mem];
int string_free_index;

char *readi_to_string(char *);
Object readi_string(char *);
Object readi_symbol(char *);
Object readi_atom(char *);

#endif
