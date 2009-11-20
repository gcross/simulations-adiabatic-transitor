#@+leo-ver=4-thin
#@+node:gcross.20091120111528.1231:@thin Makefile
#@@language Makefile
#@@tabwidth 4

all: simulate-adiabatic

VMPSROOT = ${HOME}/Projects/QC/VMPS
LIBS = -L${VMPSROOT}/lib -lvmps -llapack -larpack -lblas -lgfortran
PACKAGES = -package mtl
HASKINT = -i${VMPSROOT}/haskint
HFLAGS = -O2 -fvia-C -optc=-O3

GHCMAKE = ghc ${HFLAGS} ${PACKAGES} ${LIBS} ${HASKINT}

%: %.hs Makefile
	${GHCMAKE} $< -o $@
#@-node:gcross.20091120111528.1231:@thin Makefile
#@-leo
