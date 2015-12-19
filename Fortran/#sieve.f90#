module sieve_mod
    integer, parameter :: idp=selected_int_kind(15)

contains
    subroutine gen_primes(N, primes, nfound)
    ! generate primes up to N
    ! store in primes
    ! return number found in nfound
      integer(idp) N
      integer(idp), intent(out) :: primes(:)
      integer, intent(out) :: nfound
      integer(idp) :: i
      
      logical  :: is_prime(N)  
      is_prime = .true.
      is_prime(1) =.false.
      do i = 2, int (sqrt (real (N)))
         if (is_prime(i)) is_prime (i * i : N : i) = .false.
      end do
      nfound = 0
      do i=1,N
         if (is_prime(i)) then
            nfound = nfound + 1
            primes(nfound) = i
         end if
      end do
    end subroutine gen_primes
    integer function upper_bound(N)
      ! calculate upper bound on primes below N
      integer(idp) N
      upper_bound = int(1.3*N*log(real(N)))
    end function upper_bound
    
end module
