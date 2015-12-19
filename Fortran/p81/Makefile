FC=gfortran
FCFLAGS=-fopenmp -I./modules
% :: %.f90
	$(FC) $(FCFLAG) $^ -o $@
%.o :: %.f90
	$(FC) $(FCFLAGS) -c $<





