CHICKEN_C = csc

MODULES = sexc modules templates utils fmt-c
OBJ = $(MODULES:%=%.o)

sexc: main.o $(OBJ)
	$(CHICKEN_C) $^ -o $@

main.o: main.scm
	$(CHICKEN_C) $< -c -o $@

templates.o: templates.scm
	$(CHICKEN_C) $< -c -o $@ -compile-syntax

%.o: %.scm
	$(CHICKEN_C) $< -e -c -o $@

sex-tests: $(OBJ) tests/run.scm
	$(CHICKEN_C) tests/run.scm -c -o sex-tests.o
	$(CHICKEN_C) $(OBJ) sex-tests.o -o sex-tests

clean:
	rm -f $(OBJ) sexc sex-tests main.o sex-test.o
