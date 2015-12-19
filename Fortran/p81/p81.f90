program p81
  integer, parameter ::MSIZE = 80;
  dimension mat(MSIZE, MSIZE);
  open(unit=1, file="p081_matrix.txt")
  read(1, *) mat
  do i=MSIZE,1,-1
     do j=MSIZE,1,-1
        if (MSIZE == j .and. MSIZE == i) then
           cycle
        else if (MSIZE == j) then
           mat(i,j) = mat(i,j) + mat(i+1,j)
        else if (MSIZE == I) then
           mat(i,j) = mat(i,j) + mat(i, j+1)
        else
           mat(i,j) = mat(i,j) + min(mat(i+1,j), mat(i,j+1))
        end if
     end do
  end do
  print*, mat(1,1)
end program p81
