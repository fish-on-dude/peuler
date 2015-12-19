program p8
  integer(8) N(1000)
  integer(8) p(1000)

  ispan = 13
  p = 0
  
  do i=1,20
    read(10, '(50i1)') N((i-1)*50+1:i*50)
  end do
  
  !$omp parallel do default(none) &
  !$omp& shared(p,N,ispan)private(i)
  do i=1,1000-ispan+1
     p(i) = product(N(i:i+ispan-1))
  end do
  !$omp end parallel do
  m = maxloc(p,1)
  print*, p(m)
end program p8
