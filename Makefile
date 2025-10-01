CHICKEN_C = csc
CSC_FLAGS += -K prefix
CHICKEN_INSTALL = chicken-install

MODULES = sexc fmt-c fmt-c-writer semen sex-macros sex-modules sex-reader utils
OBJ = $(MODULES:%=%.o)

DEPSFILE = dependencies.txt
DEPSLOCK = .eggs.lock

DEPENDENCIES = $(shell cat $(DEPSFILE))
CHICKEN_EXTERNAL_REPOSITORY := $(shell chicken-install -repository)

export CHICKEN_EGG_CACHE=$(abspath .eggs)/cache
export CHICKEN_REPOSITORY_PATH=$(abspath .eggs):$(CHICKEN_EXTERNAL_REPOSITORY)
export CHICKEN_INSTALL_REPOSITORY=$(abspath .eggs)

all: sexc

sexc: main.o $(OBJ)
	$(CHICKEN_C) $(CSC_FLAGS) $^ -o $@

main.o: main.scm
	$(CHICKEN_C) $(CSC_FLAGS) $< -c -o $@

%.o: %.scm
	$(CHICKEN_C) $(CSC_FLAGS) $< -e -c -o $@

sex-tests: $(OBJ) tests/*.scm
	cd ./tests && $(CHICKEN_C) $(CSC_FLAGS) run.scm -c -o sex-tests.o
	$(CHICKEN_C) $(CSC_FLAGS) $(OBJ) ./tests/sex-tests.o -o sex-tests

run-tests: sex-tests
	./sex-tests

.eggs.lock: $(DEPSFILE)
	env | grep CHICKEN
	$(CHICKEN_INSTALL) $(DEPENDENCIES)
	touch $(DEPSLOCK)

deps: $(DEPSLOCK)

clean:
	rm -f $(OBJ) sexc sex-tests main.o ./tests/sex-tests.o

depclean:
	rm -rf .eggs $(DEPSLOCK)

.PHONY: all deps depclean clean
