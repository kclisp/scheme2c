#include <stdio.h>

typedef double Object;

//type tests
#define exp_ones ((long)0x7FF << 52)

long exponent_only(Object obj) {
  return (long)obj & exp_ones;
}

long tag(Object obj) {

}

int float_typep(Object obj) {
  return exponent_only(obj) != exp_ones;
}

int int_typep(Object obj) {
  //return !float_typep(obj) && 
}

void test_types() {
  Object obj = 0.5;
  
 label1:
  printf("%lf: float? %d address %p\n", obj, float_typep(obj), &obj);
  printf("label1: %p\n", &&label1);

  char *ptr = &&label1;
  if (obj == 0.5) {
    obj = 0;
    goto *ptr;
  }
}

int main() {
  test_types();
  
  return 0;
}
