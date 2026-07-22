CHICKEN_C = csc
CSC_FLAGS += -K prefix -static
MODULE_FLAGS = -emit-all-import-libraries -module-registration -c

ROOT = ../..

# sextest reuses sexc's reader (and its utils dependency) instead of
# duplicating the S-expression reader. The module wrappers here include
# the shared sources from the project root.

sextest: sextest.scm reader.o utils.o
	$(CHICKEN_C) $(CSC_FLAGS) sextest.scm -o sextest -link reader,utils

utils.o: utils.module.scm $(ROOT)/utils.scm
	$(CHICKEN_C) $(CSC_FLAGS) $(MODULE_FLAGS) utils.module.scm -o utils.o -unit utils

reader.o: reader.module.scm $(ROOT)/reader.scm utils.o
	$(CHICKEN_C) $(CSC_FLAGS) $(MODULE_FLAGS) reader.module.scm -o reader.o -unit reader -link utils

clean:
	rm -f *.o *.import.scm *.link sextest

.PHONY: clean
