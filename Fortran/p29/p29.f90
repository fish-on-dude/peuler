program p29
  integer, parameter :: MAXLIM=100;
  logical sieve(2:MAXLIM, 2:MAXLIM)
  !sieve = .TRUE.
  icounter = 0
  do 1 i=2,MAXLIM
     do 1 j=2,MAXLIM
        icounter = icounter + 1
        if (i*i > MAXLIM) cycle
        do 1 k=2,j-1
           if (i**k>MAXLIM)exit
           if(mod(j,k)/=0)cycle
           m=j/k ! i^j = i^(m*k) = (i^k)^m

           write(*,10) i, j, k, i**k, m
10         format(i0,'^',i0, ' == (',i0,')',i0,'^',i0)
           !sieve(i**k,m)=.FALSE.
           icounter = icounter - 1;
1         continue
           
   print*, icounter !count(sieve)
           
           
end program
