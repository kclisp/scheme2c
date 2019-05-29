# using the makefile in ../c
%:
	$(MAKE) -C ../c/ ../example/$@.out

.PHONE: clean
clean:
	rm -f *~ *.o *.out
	$(MAKE) -C ../c/ clean
