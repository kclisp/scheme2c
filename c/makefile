CC=gcc
CFLAGS=-I.
DEPS = object.h cons.h
OBJ = test.o object.o cons.o

a.out: $(OBJ)
	$(CC) -o $@ $^ $(CFLAGS)

%.o: %.c $(DEPS)
	$(CC) -c -o $@ $< $(CFLAGS)


