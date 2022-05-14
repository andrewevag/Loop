INTERPRETER = ghci
HC = ghc
BINS = ptl
INCLUDE = -I$(PWD)/
default: $(BINS)
	$(MAKE) -C loop
	
ptl: PascaltoLoop.hs 
	$(HC) -o $@ $< RPtoLoop.hs ./loop/Loop.hs ./Pascal/ReducedPascal
interpret:
	$(INTERPRETER)

clean:
	$(RM) $(BINS) *.hi *.o