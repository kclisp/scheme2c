#include "boolean.h"
#include "library.h"

int truep(Object a) {
  return !falsep(a);
}
int falsep(Object a) {
  return eqp(a, false);
}
