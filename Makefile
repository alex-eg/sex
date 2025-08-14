CHICKEN_C = csc
CHICKEN_INSTALL = chicken-install

MODULES = sexc sex-macros sex-modules utils fmt-c
OBJ = $(MODULES:%=%.o)

DEPSFILE = dependencies.txt
DEPSLOCK = .eggs.lock

DEPENDENCIES = $(shell cat $(DEPSFILE))
CHICKEN_EXTERNAL_REPOSITORY := $(shell chicken-install -repository)

export CHICKEN_EGG_CACHE=$(shell realpath ".eggs")/cache
export CHICKEN_REPOSITORY_PATH=$(shell realpath ".eggs"):$(CHICKEN_EXTERNAL_REPOSITORY)
export CHICKEN_INSTALL_REPOSITORY=$(shell realpath ".eggs")

all: sexc

sexc: main.o $(OBJ)
	$(CHICKEN_C) $^ -o $@

main.o: main.scm
	$(CHICKEN_C) $< -c -o $@

%.o: %.scm
	$(CHICKEN_C) $< -e -c -o $@

sex-tests: $(OBJ) tests/run.scm
	$(CHICKEN_C) tests/run.scm -c -o sex-tests.o
	$(CHICKEN_C) $(OBJ) sex-tests.o -o sex-tests

run-tests: sex-tests
	./sex-tests

.eggs.lock: $(DEPSFILE)
	env | grep CHICKEN
	$(CHICKEN_INSTALL) $(DEPENDENCIES)
	touch $(DEPSLOCK)

deps: $(DEPSLOCK)

clean:
	rm -f $(OBJ) sexc sex-tests main.o sex-tests.o

depclean:
	rm -rf .eggs

.PHONY: all deps depclean clean
