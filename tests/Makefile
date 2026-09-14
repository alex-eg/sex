CHICKEN_C = csc

CSC_FLAGS += -K prefix -static
MODULE_FLAGS = -emit-all-import-libraries -module-registration -c

MODULES = utils types sex-macros reader sex-modules semen sex-fmt-c fmt-c-writer sexc
SEX_OBJ = $(MODULES:%=%.o)

TESTS = basic semen reader fmt-c-writer utils line-directives codegen args types
TEST_SRCS = $(TESTS:%=%.scm)

sex-tests: run.scm $(TEST_SRCS) $(SEX_OBJ)
	$(CHICKEN_C) $(CSC_FLAGS) run.scm -o sex-tests -link sexc

#------------------------------------------------------------------

utils.o: utils.module.scm ../utils.scm
	$(CHICKEN_C) $(CSC_FLAGS) $(MODULE_FLAGS) utils.module.scm -o utils.o -unit utils

types.o: types.module.scm ../types.scm
	$(CHICKEN_C) $(CSC_FLAGS) $(MODULE_FLAGS) types.module.scm -o types.o -unit types

sex-macros.o: sex-macros.module.scm ../sex-macros.scm
	$(CHICKEN_C) $(CSC_FLAGS) $(MODULE_FLAGS) sex-macros.module.scm -o sex-macros.o -unit sex-macros

reader.o: reader.module.scm ../reader.scm utils.o
	$(CHICKEN_C) $(CSC_FLAGS) $(MODULE_FLAGS) reader.module.scm -o reader.o -unit reader -link utils

sex-modules.o: sex-modules.module.scm ../sex-modules.scm reader.o utils.o
	$(CHICKEN_C) $(CSC_FLAGS) $(MODULE_FLAGS) sex-modules.module.scm -o sex-modules.o -unit sex-modules -link reader,utils

semen.o: semen.module.scm ../semen.scm sex-macros.o sex-modules.o types.o utils.o
	$(CHICKEN_C) $(CSC_FLAGS) $(MODULE_FLAGS) semen.module.scm -o semen.o -unit semen -link sex-macros,sex-modules,types,utils

sex-fmt-c.o: ../sex-fmt-c.scm
	$(CHICKEN_C) $(CSC_FLAGS) $(MODULE_FLAGS) ../sex-fmt-c.scm -o sex-fmt-c.o -unit sex-fmt-c

fmt-c-writer.o: fmt-c-writer.module.scm ../fmt-c-writer.scm sex-fmt-c.o utils.o
	$(CHICKEN_C) $(CSC_FLAGS) $(MODULE_FLAGS) fmt-c-writer.module.scm -o fmt-c-writer.o -unit fmt-c-writer -link sex-fmt-c,utils

sexc.o: sexc.module.scm types.o ../sexc.scm fmt-c-writer.o sex-macros.o sex-modules.o reader.o semen.o utils.o
	$(CHICKEN_C) $(CSC_FLAGS) $(MODULE_FLAGS) sexc.module.scm -o sexc.o -unit sexc -link fmt-c-writer,sex-macros,sex-modules,reader,semen,types,utils

clean:
	rm -f $(SEX_OBJ)
	rm -f *.import.scm
	rm -f *.link
	rm -f sex-tests
