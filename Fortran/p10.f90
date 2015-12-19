program p10
  ! find sum of all primes below a given number
  use sieve_mod
  use timer
  integer(idp) N
  integer(idp), allocatable :: primes(:)
  print*, 'N: '

  read(*,*) N
  call tic()
  iub = upper_bound(N)
  write(*,1) 'upper bound is', iub
  allocate(primes(iub))

  call gen_primes(N, primes, nfound)
  write(*,1) 'found         ', nfound
  write(*,1) 'sum           ', sum(primes(1:nfound))
  call toc()
  call disptime()
1 format(A20, I20)
  
end program p10
