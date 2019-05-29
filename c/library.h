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
Object numequal(Object);

//list - wrappers
Object consl(Object);
Object carl(Object);
Object cdrl(Object);
Object nullpl(Object);

//output
Object display(Object);
void displayi(Object);

//time
Object clockl(Object);

#endif
