CC=gcc
CFLAGS=-I.
DEPS = object.h cons.h environment.h procedure.h library.h
OBJ = object.o cons.o environment.o procedure.o library.o

a.out: test.o $(OBJ)
	$(CC) -o $@ $^ $(CFLAGS)

# producing from template
%.out: %.o $(OBJ)
	$(CC) -o $@ $^ $(CFLAGS)

%.o: %.c $(DEPS)
	$(CC) -c -o $@ $< $(CFLAGS)

clean:
	rm -f *~ *.o *.out
