#ifndef LIBRARY_H
#define LIBRARY_H

#include "object.h"

//predicates
//equality
int eqp(Object, Object);

//arithemtic
Object add(Object);

//output
Object display(Object);
void displayi(Object);
#endif
