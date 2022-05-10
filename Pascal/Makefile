PC = fpc
SOURCE = $(wildcard *.pas)
BINS = $(patsubst %.pas, %, $(SOURCE))

default: $(BINS)


% : %.pas
	$(PC) $^

clean: 
	$(RM) *.o $(BINS)