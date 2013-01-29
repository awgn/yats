# (C) 2011-13 Nicola Bonelli <nicola.bonelli@antifork.org>
#

GHCFLAGS= --make -W -O2

INSTDIR=/usr/local

HC=ghc

.PHONY: all clean

all: yats

Network/PFq.hs: 
		$(HSC) Network/PFq.hsc 

yats: yats.hs  
		$(HC) $(GHCFLAGS) $(LIBS) yats.hs -o $@

install: all
		cp yats  ${INSTDIR}/bin/
        
clean:
	   @rm -f yats
	   @rm -f *.o *.hi 
