CHICKEN_C = csc

sexc: sexc.scm
	$(CHICKEN_C) $< -o $@

clean:
	rm -f $(OBJ) sexc
