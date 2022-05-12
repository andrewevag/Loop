BINS = loop

default: $(BINS)


loop: Loop.hs Main.hs
	ghc -o $@ $^

run : loop
	./run.sh
	
clean:
	$(RM) *.hi *.o $(BINS)

cbins: 
	$(MAKE) -C cprograms