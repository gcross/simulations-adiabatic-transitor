#@+leo-ver=4-thin
#@+node:gcross.20091120111528.1231:@thin Makefile
#@@language Makefile
#@@tabwidth 4

all: \
	programs/simulate-adiabatic-constant-angle \
	programs/bitslayer-adiabatic-constant-angle-sweep \
	programs/condor-adiabatic-constant-angles-sweep \
	programs/plot-adiabatic-constant-angle-gap \
	programs/simulate-adiabatic-random-angles \
	programs/condor-adiabatic-random-angles-sweep

LIBS = -larpack -llapack -lblas -lgfortran

HFLAGS = -O2 -fvia-C -optc=-O3 -isources

GHCMAKE = ghc --make -static ${HFLAGS} ${LIBS}

programs/%: sources/%.hs Makefile sources/Database.hs sources/VMPSDatabase.hs
	${GHCMAKE} $< -o $@
#@-node:gcross.20091120111528.1231:@thin Makefile
#@-leo
