program p12
  ! find first triangle number with at least 500 factors
  use sieve_mod
  implicit none
  integer(idp), parameter :: Nfac = 500,maxprime=1000000
  integer(idp), allocatable:: primes(:) 
  integer(idp) np
  integer, allocatable::  pf(:), pflast(:), pfsum(:)
  integer i, nfound,ntrifac

  np = upper_bound(maxprime)
  allocate(primes(np),pf(np),pflast(np),pfsum(np))
  primes = 0
  pf = 0
  call gen_primes(maxprime, primes, nfound)

  print*, np, nfound

  i = 1
  
  pf = pfac(i)
  
  do
     i = i + 1
     pflast = pf
     pf = pfac(i)
     pfsum = pflast+pf
     pfsum(1) = pfsum(1) - 1
     ntrifac = 1
     ntrifac = product(pfsum +1)

     print*, (i-1)* i /2, ntrifac
     if (ntrifac .ge. 500) exit

  end do


  
     

contains
  
  function pfac(n)
    ! find prime factors of a number
    integer n, ntemp, i, ptest
    integer pfac(nfound)
    ntemp = n
    pfac = 0
    do i=1,nfound
       do while (mod(ntemp,primes(i))==0)
          !print*, ntemp, '=', primes(i) , '*', ntemp/primes(i)
          pfac(i) = pfac(i) +1
          ntemp = ntemp / primes(i)
       end do
       if (ntemp ==1) exit
    end do
    !print*, pack(primes**pfac, pfac > 0)
    ptest = product(primes**pfac, 1, pfac > 0)
    if (ptest /= n) then
       print*,'wrong answer: ', n, '/=', ptest
       print*,pfac
       print*, primes(1:5)
       stop
    end if
    
  end function pfac

end program
    
  

  
 
