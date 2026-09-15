# Multi-module linking.
#
# Modules are only testable end to end, and nothing else in the suite
# links more than one translation unit. Three things have to hold at
# once: an imported `pub fn' comes out as a prototype with external
# linkage, an imported `pub var' as an extern declaration, and the
# module's object survives being passed after `--'. Get any of them
# wrong and this fails to link -- or, in the `pub var' case, links and
# quietly counts into a private copy.
#
# It also checks what only a second translation unit can check: that an
# imported type reaches the type database, by expanding a macro that
# reads the imported struct's fields.

SEXC ?= ../../sexc

EXPECTED = hello, world\nhello, sex\n2 greetings\ntext times \nmood 1

check:
	@$(SEXC) greet.sex -c -o greet.o
	@$(SEXC) greet-app.sex -o greet-app -- greet.o
	@if [ "`./greet-app`" = "`printf '$(EXPECTED)\n'`" ]; then \
		echo "modules ok"; \
	else \
		echo "modules FAILED, got:"; ./greet-app; $(MAKE) clean; exit 1; \
	fi
	@$(MAKE) --no-print-directory clean

clean:
	@rm -f greet.o greet-app

.PHONY: check clean
