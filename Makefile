CHICKEN_C ?= csc
CHICKEN_INSTALL ?= chicken-install
CHICKEN_STATUS ?= chicken-status
CSC_FLAGS += -K prefix -static
# What and why:
# -emit-all-import-libraries: Emit import-libraries for all defined modules.
#    Used to generate .import.scm files so compiler would know how to use the modules.
#    Without it, csc fails with "cannot import from undefined module" error.
# -module-registration: Always  generate  module registration code, even when
#    import libraries are emitted. Enables us to import from our modules at run time.
#    Used in macros, where we inject `(import (only sex-macros cat))' before the macro body.
#    Without it, sexc compiles scuccessfully, but is unable to process macros, failing with
#    "during expansion of (import ...) - cannot import from undefined module: sex-macros"
#     error.
# -c: Stop after compilation to object files. This one is obvious.

# GNU directory variables. Command line overrides, e.g.
#   make prefix=$(HOME)/.local install
#   make DESTDIR=/tmp/stage prefix=/usr install
prefix = /usr/local
exec_prefix = $(prefix)
bindir = $(exec_prefix)/bin
INSTALL = install
INSTALL_PROGRAM = $(INSTALL)

MODULE_FLAGS = -emit-all-import-libraries -module-registration -c

# Order matters, since module check correctness on compilation
MODULES = utils types sex-macros reader sex-modules semen sex-fmt-c fmt-c-writer sexc
OBJ = $(MODULES:%=%.o)

DEPSFILE = dependencies.txt
DEPSLOCK = eggs.lock
EGGS_DIR := $(abspath .eggs)

# Compiler's own repository (types.db, chicken.*), ignoring ambient venv vars.
SYSTEM_CHICKEN_REPO := $(shell env -u CHICKEN_INSTALL_REPOSITORY -u CHICKEN_REPOSITORY_PATH -u CHICKEN_EGG_CACHE -u CHICKEN_INSTALL_PREFIX $(CHICKEN_INSTALL) -repository 2>/dev/null)
CHICKEN_ABI := $(notdir $(SYSTEM_CHICKEN_REPO))
EGGS_STAMP := $(EGGS_DIR)/.installed-$(CHICKEN_ABI)

export CHICKEN_EGG_CACHE := $(EGGS_DIR)/cache
export CHICKEN_INSTALL_REPOSITORY := $(EGGS_DIR)
export CHICKEN_INSTALL_PREFIX := $(EGGS_DIR)
export CHICKEN_REPOSITORY_PATH := $(EGGS_DIR):$(SYSTEM_CHICKEN_REPO)

all: sexc

sexc: $(EGGS_STAMP) $(OBJ) main.scm
	$(CHICKEN_C) $(CSC_FLAGS) main.scm -o sexc-tmp -link sexc
        # otherwise csc hangs, probably because it tries to compile to sexc.o first
	mv sexc-tmp sexc

$(OBJ): $(EGGS_STAMP)

#------------------------------------------------------------------

utils.o: utils.module.scm utils.scm
	$(CHICKEN_C) $(CSC_FLAGS) $(MODULE_FLAGS) utils.module.scm -o utils.o -unit utils

types.o: types.module.scm types.scm
	$(CHICKEN_C) $(CSC_FLAGS) $(MODULE_FLAGS) types.module.scm -o types.o -unit types

sex-macros.o: sex-macros.module.scm sex-macros.scm
	$(CHICKEN_C) $(CSC_FLAGS) $(MODULE_FLAGS) sex-macros.module.scm -o sex-macros.o -unit sex-macros

reader.o: reader.module.scm reader.scm utils.o
	$(CHICKEN_C) $(CSC_FLAGS) $(MODULE_FLAGS) reader.module.scm -o reader.o -unit reader -link utils

sex-modules.o: sex-modules.module.scm sex-modules.scm reader.o utils.o
	$(CHICKEN_C) $(CSC_FLAGS) $(MODULE_FLAGS) sex-modules.module.scm -o sex-modules.o -unit sex-modules -link reader,utils

semen.o: semen.module.scm semen.scm sex-macros.o sex-modules.o types.o utils.o
	$(CHICKEN_C) $(CSC_FLAGS) $(MODULE_FLAGS) semen.module.scm -o semen.o -unit semen -link sex-macros,sex-modules,types,utils

sex-fmt-c.o: sex-fmt-c.scm
	$(CHICKEN_C) $(CSC_FLAGS) $(MODULE_FLAGS) sex-fmt-c.scm -o sex-fmt-c.o -unit sex-fmt-c

fmt-c-writer.o: fmt-c-writer.module.scm fmt-c-writer.scm sex-fmt-c.o utils.o
	$(CHICKEN_C) $(CSC_FLAGS) $(MODULE_FLAGS) fmt-c-writer.module.scm -o fmt-c-writer.o -unit fmt-c-writer -link sex-fmt-c,utils

sexc.o: sexc.module.scm types.o sexc.scm fmt-c-writer.o sex-macros.o sex-modules.o reader.o semen.o utils.o
	$(CHICKEN_C) $(CSC_FLAGS) $(MODULE_FLAGS) sexc.module.scm -o sexc.o -unit sexc -link fmt-c-writer,sex-macros,sex-modules,reader,semen,types,utils

# Unit testing
sex-tests: $(EGGS_STAMP)
	$(MAKE) -C ./tests sex-tests
	cp ./tests/sex-tests ./

sextest: $(EGGS_STAMP)
	$(MAKE) -C ./tools/sextest sextest
	cp ./tools/sextest/sextest .

SEX_TEST_PROGRAMS = hello-world lists comments unicode serialize features

# Multi-module linking is checked end to end; see tests/modules/Makefile.
check-modules: sexc
	@$(MAKE) --no-print-directory -C ./tests/modules check SEXC=../../sexc

# The failure paths are checked end to end; see tests/exit-code/Makefile.
check-exit-code: sexc
	@$(MAKE) --no-print-directory -C ./tests/exit-code check SEXC=../../sexc

run-tests: sexc sex-tests sextest
	./sex-tests && ./sextest --sexc=./sexc $(SEX_TEST_PROGRAMS:%=./tests/sex-programs/%.sex) && $(MAKE) check-modules && $(MAKE) check-exit-code

install: all installdirs
	$(INSTALL_PROGRAM) sexc $(DESTDIR)$(bindir)/sexc

install-strip:
	$(MAKE) INSTALL_PROGRAM='$(INSTALL_PROGRAM) -s' install

installdirs:
	$(INSTALL) -d $(DESTDIR)$(bindir)

uninstall:
	rm -f $(DESTDIR)$(bindir)/sexc

# eggs.lock is the pin file (chicken-status -list). Install from it;
# do not float versions on a normal build. Regenerating the lock:
#   make deps-update
$(EGGS_STAMP): $(DEPSLOCK)
	mkdir -p $(CHICKEN_EGG_CACHE)
	$(CHICKEN_INSTALL) -force -from-list $(DEPSLOCK)
	touch $@

deps: $(EGGS_STAMP)

deps-update: $(DEPSFILE)
	rm -rf $(EGGS_DIR)
	mkdir -p $(CHICKEN_EGG_CACHE)
	$(CHICKEN_INSTALL) -force $$(cat $(DEPSFILE))
	CHICKEN_REPOSITORY_PATH=$(EGGS_DIR) $(CHICKEN_STATUS) -list | sort > $(DEPSLOCK).tmp
	mv $(DEPSLOCK).tmp $(DEPSLOCK)
	touch $(EGGS_STAMP)

clean:
	rm -f $(OBJ) main.o
	rm -f *.import.scm
	rm -f *.link
	rm -f sexc sex-tests sextest
	$(MAKE) -C ./tests clean
	$(MAKE) -C ./tests/modules clean
	$(MAKE) -C ./tools/sextest clean

deps-clean:
	rm -rf $(EGGS_DIR)

.PHONY: all check run-tests check-modules check-exit-code install install-strip installdirs
	uninstall deps deps-update clean depsclean sex-tests sextest
