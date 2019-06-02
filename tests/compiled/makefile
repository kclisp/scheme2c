C_DIR = ../../c/
HERE_FROM_C_DIR = ../tests/compiled/
OPTS = 

# use the makefile in ../../c
%:
	$(MAKE) -C $(C_DIR) $(OPTS) $(HERE_FROM_C_DIR)$@.out

profile:
	$(eval OPTS += profile)
ndebug:
	$(eval OPTS += ndebug)
debug:
	$(eval OPTS += debug)
speed:
	$(eval OPTS += speed)
size:
	$(eval OPTS += size)

.PHONY: clean
clean:
	rm -f *~ *.o *.out
	$(MAKE) -C $(C_DIR) clean
