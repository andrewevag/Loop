BINS = loop

default: $(BINS)


loop: Loop.hs
	ghc -o $@ $<

clean:
	$(RM) *.hi *.o $(BINS)

cbins: 
	$(MAKE) -C cprograms