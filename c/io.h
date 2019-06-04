#ifndef IO_H
#define IO_H

#include "object.h"

//currently, this value isn't used
#define END_OF_INPUT (Object)((uint64_t)-1 + exp_ones + bool_tag)
#define READ_BUF_SIZE 1 << 7

void displayi(Object);
Object readi(char *);
Object readi_list(char *);

#define max_string_mem 1 << 11
char string_mem[max_string_mem];
int string_free_index;

char *readi_to_string(char *);
Object readi_string(char *);
Object readi_symbol(char *);
Object readi_atom(char *);



#endif
