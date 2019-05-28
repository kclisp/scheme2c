CC=gcc
CFLAGS=-I.
DEPS = object.h cons.h environment.h procedure.h library.h symbol.h boolean.h
OBJ = object.o cons.o environment.o procedure.o library.o symbol.o boolean.c

test.out: test.o $(OBJ)
	$(CC) -o $@ $^ $(CFLAGS)

# producing from template
%.out: %.o $(OBJ)
	$(CC) -o $@ $^ $(CFLAGS)

%.o: %.c $(DEPS)
	$(CC) -c -o $@ $< $(CFLAGS)

.PHONY: clean

clean:
	rm -f *~ *.o *.out
