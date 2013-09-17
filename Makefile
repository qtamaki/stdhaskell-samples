# Makefile for stdhaskell sample programs

TARGETS = hello cat cat2 cat3 head head2 tail tail2 tac tac2 \
          sort sort2 uniq uniq2 echo fgrep fgrep2 fgrep3 grep qsort \
          countbyte countword countline countline2 countline3 \
          tr expand0 expand expand2 catn catn2 tarai fold swapa \
	  todos tounix tounix2 yes wc parseconfig glob line \
	  tail3 expand3

HC      = ghc
HCFLAGS = -W -fno-warn-unused-matches
JAVAC   = javac
CC      = gcc
CFLAGS  = -Wall -O2

.SUFFIXES:
.SUFFIXES: .hs .
.hs:
	$(HC) $(HCFLAGS) $< -o $@

default: haskell

all: haskell Tarai.class tarai-c

haskell: $(TARGETS)

glob: glob.hs
	$(HC) --make $(HCFLAGS) glob.hs -o glob

Tarai.class: Tarai.java
	$(JAVAC) $<

tarai-c: tarai.c
	$(CC) $(CFLAGS) $< -o $@

clean:
	rm -f $(TARGETS) *.o *.hi *.exe *.class tarai-c
