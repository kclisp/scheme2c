#include "stack.h"
#include <assert.h>

Object stack[stack_max_level];
static int stack_index = 0;

void save(Object obj) {
  assert(stack_index < stack_max_level);
  stack[stack_index++] = obj;
}
Object restore() {
  assert(stack_index > 0);
  return stack[--stack_index];
}
