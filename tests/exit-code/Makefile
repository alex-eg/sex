# The failure paths.
#
# Both need a process to show themselves, so neither fits the unit
# suite: sexc has to fail when cc fails -- exiting 0 after a failed
# compile makes every driver, sextest included, read it as success --
# and it has to clean up its temporary .c when its own emission throws.
#
# Everything is built inside a scratch TMPDIR, so nothing is left here
# to clean up.

SEXC ?= ../../sexc

check:
	@d=`mktemp -d`; \
	TMPDIR=$$d $(SEXC) nested-pointer.sex -o $$d/out >/dev/null 2>&1; \
	if [ -n "`find $$d -name '*.c'`" ]; then \
		echo "exit code FAILED: the temporary .c survived a failed emission"; \
		rm -rf $$d; exit 1; \
	fi; \
	if $(SEXC) hello.sex -o $$d/out -- -no-such-cc-flag >/dev/null 2>&1; then \
		echo "exit code FAILED: sexc reported success after cc failed"; \
		rm -rf $$d; exit 1; \
	fi; \
	rm -rf $$d; echo "exit code ok"

.PHONY: check
