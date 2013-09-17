# LazyLines Makefile

HMAKE    = $(HC) --make $(HCFLAGS) -cpp $(CPPFLAGS) $(PACKAGES)
HC       = ghc
HCFLAGS  = -W -fno-warn-unused-matches
CPPFLAGS = -DPOSIX
#CPPFLAGS = -DWIN32
PACKAGES =
TARGET   = index.cgi

default: build-$(TARGET)

all: build-$(TARGET) build-compile build-showconfig

build-$(TARGET):
	$(HMAKE) CGIMain.hs -o $(TARGET)

build-compile:
	$(HMAKE) compile.hs -o compile

build-showconfig:
	$(HMAKE) showconfig.hs -o showconfig

clean:
	rm -f $(TARGET) compile showconfig *.o *.hi core

win32binary:
	make filter
	make all CPPFLAGS=-DWIN32
	make exe

filter:
	for i in *.hs; do \
	    fgrep -v '#@@' $$i > tmp && mv tmp $$i; \
	done

exe: server.exe cgirunner.exe

server.exe: server.exy
	/cygdrive/c/Program\ Files/ruby-1.8/bin/exerb.bat server.exy
server.exy: server.rb
	/cygdrive/c/Program\ Files/ruby-1.8/bin/ruby -rexerb/mkexy server.rb --quit

cgirunner.exe: cgirunner.exy
	/cygdrive/c/Program\ Files/ruby-1.8/bin/exerb.bat cgirunner.exy
cgirunner.exy: cgirunner.rb
	-echo | /cygdrive/c/Program\ Files/ruby-1.8/bin/ruby -rexerb/mkexy cgirunner.rb
