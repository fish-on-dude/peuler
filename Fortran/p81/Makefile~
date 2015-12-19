FC=gfortran
FCFLAGS=-fopenmp -I./modules
% :: %.f90
	$(FC) $(FCFLAG) $^ -o $@
%.o :: %.f90
	$(FC) $(FCFLAGS) -c $<

all: p10

p10: timer.o sieve.o p10.o
	$(FC) $(FCFLAGS) $^ -o $@
p12: timer.o sieve.o p12.o
	$(FC) $(FCFLAGS) $^ -o $@


sieve.o: sieve.f90

timer.o: timer.f90



