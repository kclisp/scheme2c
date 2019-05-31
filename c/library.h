#ifndef LIBRARY_H
#define LIBRARY_H

#include "object.h"

//addresses are exactly equal
inline int eqp(Object a, Object b) {return a.u == b.u;}

#define PRIMITIVE(x, y) Object y(Object);
#include "primitives.def"

void displayi(Object);

#endif
