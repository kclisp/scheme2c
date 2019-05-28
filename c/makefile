CC=gcc
CFLAGS=-I.
DEPS = object.h cons.h environment.h library.h
OBJ = test.o object.o cons.o environment.o library.o

a.out: $(OBJ)
	$(CC) -o $@ $^ $(CFLAGS)

%.o: %.c $(DEPS)
	$(CC) -c -o $@ $< $(CFLAGS)


