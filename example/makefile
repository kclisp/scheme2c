OPTS = 

# use the makefile in ../c
%:
	$(MAKE) -C ../c/ $(OPTS) ../example/$@.out

profile:
	$(eval OPTS += profile)
ndebug:
	$(eval OPTS += ndebug)
speed:
	$(eval OPTS += speed)
size:
	$(eval OPTS += size)

.PHONE: clean
clean:
	rm -f *~ *.o *.out
	$(MAKE) -C ../c/ clean
