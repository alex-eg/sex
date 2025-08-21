CHICKEN_C = csc

MODULES = sexc sex-macros sex-modules utils fmt-c
OBJ = $(MODULES:%=%.o)

sexc: main.o $(OBJ)
	$(CHICKEN_C) $^ -o $@

main.o: main.scm
	$(CHICKEN_C) $< -c -o $@

%.o: %.scm
	$(CHICKEN_C) $< -e -c -o $@

sex-tests: $(OBJ) tests/*.scm
	cd ./tests && $(CHICKEN_C) run.scm -c -o sex-tests.o
	$(CHICKEN_C) $(OBJ) ./tests/sex-tests.o -o sex-tests

clean:
	rm -f $(OBJ) sexc sex-tests main.o ./tests/sex-tests.o
