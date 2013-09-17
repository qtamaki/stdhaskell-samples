HMAKE   = $(HC) --make
HC      = ghc
HCFLAGS = -W -fno-warn-unused-matches

compile: compile.hs LineParser.hs MyParser.hs
	$(HMAKE) $(HCFLAGS) compile.hs -o $@

clean:
	rm -f compile *.o *.hi *.exe
