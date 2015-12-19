program p5
  ! find smallest number evenly divisible by all numbers 1..20
  integer, allocatable :: primes(:)
  integer(8) N, ifac
  read (*,*) N
  print*, N
  call sieve(N)

  write(*, '(i0)') primes
  np = size(primes)
  ifac = 1
  do i=1,np
     p = primes(i)
     k = int(log(real(N))/log(real(p)))
     pk = p**k
     ifac = ifac*pk
     print*, p, k, pk, ifac
  end do
  print*, ifac
contains
  subroutine sieve(N)
    integer(8) :: N
    logical  :: is_prime(N)  
    is_prime = .true.
    is_prime(1) =.false.
    do i = 2, int (sqrt (real (N)))
      if (is_prime(i)) is_prime (i * i : N : i) = .false.
    end do
    allocate(primes(count(is_prime)) )
    primes = pack([(i,i=1,N)],is_prime)   
end subroutine  
end program p5
