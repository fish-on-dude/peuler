module timer
  implicit none
  real cpu_start, cpu_end
  integer(8) system_start, system_end, clockrate
  contains

  subroutine tic()
    call system_clock(system_start, clockrate)
    call cpu_time(cpu_start)
  end subroutine tic

  subroutine toc()
    call system_clock(system_end, clockrate)
    call cpu_time(cpu_end)
  end subroutine toc
  
  
  subroutine disptime()
    print*, 'cputime = ', (cpu_end - cpu_start)*1000, 'ms'
    print*, 'wall = ',( real(system_end-system_start)/clockrate)*1000, 'ms'
   ! print*, 'CPU: ',fmttime(int(1000*(cpu_end-cpu_start),8))
   ! print*, 'wall:',fmttime(system_end-system_start)
  end subroutine disptime
  

  function fmttime(ms)
    character(20) fmttime
    integer(8), intent(in):: ms
    integer(8):: tms, time(3)
    integer i, n
    integer, parameter:: ifac(3) = [1000, 60, 60]
    tms = ms
    print*, 'ms  = ', ms
    
    do i=1,3
       time(i) = mod(tms, ifac(i))
       tms = tms/ifac(i)
       print*, 'ms = ', tms
       if (tms>0) n = i+1
       if (tms == 0) exit
    end do
    select case (n)
    case(1)
       write(fmttime, 10) time(1)
    case(2)
       write(fmttime, 20) time(2), time(1)
    case(3)
       write(fmttime, 30) time(3), time(2), time(1)
    end select
10  format( i0, 'ms')
20  format(i0, '.', i4.4, 's')
30  format (i0, 'm:',i2.2, '.', i4.4,'s')

  end function fmttime

end module timer
