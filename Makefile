#@+leo-ver=4-thin
#@+node:gcross.20091120111528.1231:@thin Makefile
#@@language Makefile
#@@tabwidth 4

all: simulate-adiabatic-constant-angle # compute-energy test

LIBS = -llapack -larpack -lblas -lgfortran

HFLAGS = -O2 -fvia-C -optc=-O3

GHCMAKE = ghc --make ${HFLAGS} ${LIBS}

%: %.hs Makefile Database.hs
	${GHCMAKE} $< -o $@
#@-node:gcross.20091120111528.1231:@thin Makefile
#@-leo
