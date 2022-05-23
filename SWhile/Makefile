INTERPRETER = ghci
HC = ghc
BINS = swhile
INCLUDE = -I$(PWD)/
default: $(BINS)
	
swhile: Main.hs SWhile.hs
	$(MAKE) -C ../loop
	$(HC) -o $@ $< ../loop/Loop.hs 
interpret:
	$(INTERPRETER)

clean:
	$(RM) $(BINS) *.hi *.o