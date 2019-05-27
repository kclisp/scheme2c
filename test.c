#include <stdio.h>
#include <stdint.h>

//Object definition
union Object {
  uint64_t u;
  double d;
};

typedef union Object Object;

//nan tag components
#define exp_ones ((uint64_t)0x7FF << 52)
#define int_tag ((uint64_t)1<<51)
#define pointer_tag(x) ((uint64_t)x<<49)
#define pointer_mask pointer_tag(11)
#define cons_tag pointer_tag(00)
#define symbol_tag pointer_tag(01)
#define string_tag pointer_tag(10)
#define vector_tag pointer_tag(11)

//casts
Object integer_to_obj(uint64_t num) {
  return (Object)(num + exp_ones + int_tag);
}

//predicates
int has_flag(Object obj, long mask, long flag) {
  return (obj.u & mask) == flag;
}

int float_typep(Object obj) {
  return !has_flag(obj, exp_ones, exp_ones);
}
int int_typep(Object obj) {
  return !float_typep(obj) && has_flag(obj, int_tag, int_tag);
}
int pointer_typep(Object obj) {
  return !float_typep(obj) && !int_typep(obj);
}
int pointer_has_flag(Object obj, long flag) {
  return pointer_typep(obj) && has_flag(obj, pointer_mask, flag);
}
int cons_typep(Object obj) {
  return pointer_has_flag(obj, cons_tag);
}
int symbol_typep(Object obj) {
  return pointer_has_flag(obj, symbol_tag);
}
int string_typep(Object obj) {
  return pointer_has_flag(obj, string_tag);
}
int vector_typep(Object obj) {
  return pointer_has_flag(obj, vector_tag);
}

//type tests
void test_types() {
  Object obj = {.d = .5};
  
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
}

int main() {
  test_types();
  printf("exp_ones: %ld\n", exp_ones);
  Object temp = {.u = exp_ones};
  printf("infinity: %f\n", temp.d);
  temp.u++;
  printf("nan: %f\n", temp.d);

  temp.u--;
  temp.u += (uint64_t)1<<63;
  printf("-infinity: %f\n", temp.d);
  temp.u++;
  printf("-nan: %f\n", temp.d);
  return 0;
}
