CC=gcc
CFLAGS=-I.
DEPS = object.h cons.h environment.h procedure.h library.h symbol.h boolean.h stack.h io.h
OBJ = object.o cons.o environment.o procedure.o library.o symbol.o boolean.o stack.o io.o

test.out: test.o $(OBJ)
	$(CC) -o $@ $^ $(CFLAGS)

all: $(OBJ)

# producing from template
%.out: %.o $(OBJ)
	$(CC) -o $@ $^ $(CFLAGS)

%.o: %.c $(DEPS)
	$(CC) -c -o $@ $< $(CFLAGS)

profile:
	$(eval CFLAGS += -pg)
ndebug:
	$(eval CFLAGS += -DNDEBUG)
debug:
	$(eval CFLAGS += -g)
speed:
	$(eval CFLAGS += -O3)
size:
	$(eval CFLAGS += -Os -ffunction-sections -fdata-sections -Wl,--gc-sections)


.PHONY: clean

clean:
	rm -f *~ *.o *.out
