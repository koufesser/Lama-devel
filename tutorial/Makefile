FILES=$(wildcard *.lama)
ALL=$(sort $(FILES:.lama=))
EXTRA_TARGETS =

ifeq ($(LAMAC),)
	LAMAC := LAMA=../runtime dune exec ../src/Driver.exe --  -I ../stdlib
	EXTRA_TARGETS += ../src/lamac
endif

all: $(EXTRA_TARGETS) $(ALL)

../src/lamac:
	$(MAKE) -C ../src

%: %.lama
	$(LAMAC) $<

clean:
	$(RM) -R *.s *.o *.i *~ $(ALL)
