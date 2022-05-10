CC = gcc
CFLAGS = -g -Wall
SOURCE = $(wildcard *.c)
BINS = $(patsubst %.c, %, $(SOURCE))

all: $(BINS)

%: %.c
	$(CC) $(CFLAGS) -o $@ $<