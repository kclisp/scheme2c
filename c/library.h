#ifndef LIBRARY_H
#define LIBRARY_H

#include "object.h"

//predicates
//equality
int eqp(Object, Object);

//arithemtic
Object add(Object);
Object subtract(Object);
Object multiply(Object);

//output
Object display(Object);
void displayi(Object);
#endif
