#ifndef LIBRARY_H
#define LIBRARY_H

#include "object.h"

//addresses are exactly equal
inline int eqp(Object a, Object b) {return a.u == b.u;}

#define PRIMITIVE(scheme_name, c_name) Object c_name(Object);
#include "../data/primitives.def"
#undef PRIMITIVE

#endif
