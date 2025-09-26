CHICKEN_C = csc
CSC_FLAGS = -K prefix

MODULES = sexc fmt-c fmt-c-writer semen sex-macros sex-modules sex-reader utils
OBJ = $(MODULES:%=%.o)

sexc: main.o $(OBJ)
	$(CHICKEN_C) $(CSC_FLAGS) $^ -o $@

main.o: main.scm
	$(CHICKEN_C) $(CSC_FLAGS) $< -c -o $@

%.o: %.scm
	$(CHICKEN_C) $(CSC_FLAGS) $< -e -c -o $@

sex-tests: $(OBJ) tests/*.scm
	cd ./tests && $(CHICKEN_C) $(CSC_FLAGS) run.scm -c -o sex-tests.o
	$(CHICKEN_C) $(CSC_FLAGS) $(OBJ) ./tests/sex-tests.o -o sex-tests

clean:
	rm -f $(OBJ) sexc sex-tests main.o ./tests/sex-tests.o
