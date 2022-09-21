TESTS=$(sort $(basename $(wildcard test*.lama)))

LAMAC=dune exec ../../src/Driver.exe --

.PHONY: check $(TESTS)

check: $(TESTS)

$(TESTS): %: %.lama
	@echo $@
	LAMA=../../runtime $(LAMAC) -I .. -ds -dp $< && ./$@ > $@.log && diff $@.log orig/$@.log

clean:
	$(RM) test*.log *.s *~ $(TESTS) *.i
