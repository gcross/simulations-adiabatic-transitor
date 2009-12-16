#@+leo-ver=4-thin
#@+node:gcross.20091120111528.1231:@thin Makefile
#@@language Makefile
#@@tabwidth 4

all: \
	programs/simulate-adiabatic-constant-angle \
	programs/sweep-bitslayer-adiabatic-constant-angle \
	programs/sweep-condor-adiabatic-constant-angles \
	programs/plot-adiabatic-constant-angle-gap \
	programs/simulate-adiabatic-random-angles \
	programs/sweep-condor-adiabatic-random-angles \
	programs/plot-adiabatic-random-angle-gap \
	programs/simulate-adiabatic-restricted-random-angles \
	programs/sweep-condor-adiabatic-restricted-random-angles \
	programs/plot-adiabatic-restricted-random-angle-gap \

LIBS = 

HFLAGS = -O2 -fvia-C -optc=-O3 -isources

GHCMAKE = ghc --make ${HFLAGS} ${LIBS}

programs/%: sources/%.hs Makefile
	${GHCMAKE} $< -o $@

clean:
	rm -f programs/* sources/*.o sources/*.hi
#@-node:gcross.20091120111528.1231:@thin Makefile
#@-leo
