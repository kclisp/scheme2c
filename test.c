#include <stdio.h>
#include <stdint.h>

union Object {
  uint64_t u;
  double d;
};

typedef union Object Object;

//type tests
//components
#define exp_ones ((uint64_t)0x7FF << 52)
#define int_tag ((uint64_t)1<<51)

long exponent(Object obj) {
  return obj.u & exp_ones;
}

long tag(Object obj) {
  
}

//casts
Object integer_to_obj(uint64_t num) {
  return (Object)(num + exp_ones + int_tag);
}

//predicates
int float_typep(Object obj) {
  return exponent(obj) != exp_ones;
}
int int_typep(Object obj) {
  return !float_typep(obj);
}

void test_types() {
  Object obj = {.d = 0.5};
  
 label_1:
  printf("%f: float? %d address %p long %lx\n", obj.d, float_typep(obj), &obj, obj.u);
  printf("label_1: %p\n", &&label_1);

  char *ptr = &&label_1;
  if (obj.d >= 0.5) {
    obj.d -= 0.4;
    goto *ptr;
  } else if (obj.d >= 0.0) {
    obj.d -= 0.4;
    goto label_1;
  }
  printf("string: %p\n", "hi");
}

int main() {
  test_types();
  printf("exp_ones: %ld\n", exp_ones);
  Object temp = {.u = exp_ones};
  printf("infinity: %f\n", temp.d);
  temp.u++;
  printf("nan: %f\n", temp.d);

  temp.u--;
  temp.u += (long)1<<63;
  printf("-infinity: %f\n", temp.d);
  temp.u++;
  printf("-nan: %f\n", temp.d);
  return 0;
}
