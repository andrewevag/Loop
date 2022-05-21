INTERPRETER = ghci
HC = ghc
BINS = while
INCLUDE = -I$(PWD)/
default: $(BINS)
	
while: Main.hs While.hs
	$(MAKE) -C ../loop
	$(HC) -o $@ $< ../loop/Loop.hs 
interpret:
	$(INTERPRETER)

clean:
	$(RM) $(BINS) *.hi *.o