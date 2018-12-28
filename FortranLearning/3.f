      INCLUDE "3-Functions.f"
      PROGRAM EXERCISE31
      IMPLICIT DOUBLE PRECISION(A-Z)
      PARAMETER(pi=3.14159265, tau=pi*2) ! general purpose parameters
c Integers are only to be used when making loops; DBLE for more serious precision

c variables that I don't know the value of immediately
      CHARACTER*8 finishTime_str
      INTEGER finish(3),i
      LOGICAL :: exit = .FALSE.

c Variables whose value I already know for certain
      PARAMETER(xMi=-3.0, xMa=2.8, xStep = 0.2, A4Long=29.7)!implicitly double precisions
      INTEGER, PARAMETER :: XarySF = 10
      INTEGER, PARAMETER :: XaryMi = NINT( xMi *XarySF)
      INTEGER, PARAMETER :: XaryMa = NINT( xMa *XarySF)
      INTEGER, PARAMETER :: XaryStep=NINT(xStep*XarySF)
      INTEGER, PARAMETER :: XaryLeng=NINT((xMa-xMi)/xStep+1)

c All arocate memory for arrays and give values to x
      DOUBLE PRECISION, dimension(1:XaryLeng) :: x !this is only an intermediate array
      DOUBLE PRECISION, dimension(1:6,1:5)    :: xnew
      x(:) = (/(i, i=XaryMi,XaryMa, XaryStep)/)/DBLE(XarySF) !implicitly real x

c Decide value for each variable
      DO 20 i=1,XaryLeng
            call Gaussian0(DBLE(0),DBLE(1), x(i)) !Variable type must match, therefore the DBLE()'s'
 20   end DO

c main part of calculation
c exercise 1
      Mass1 = 1E23
      Mass2 = 12
      dist = 3E6
      number = GravitationalPull(mass1, mass2, dist)
c exercise 2
      xNew = reshape(x, (/6,5/), order = (/(i, i=2,1,-1)/)) !implicitly create an array denoting the order of xnew inline
      DO 10 i=1, 6
            print*, xNew(i,:)
 10   end DO
c exercise 3
      longSide = A4Long*(sqrt(2.0)**4)*SQRT(2.)
      DO 30 i=0,6
            call divideA(i,longSide)
 30   end DO
c exercise 4
      DO while (.not.exit)
            read(5,*) i
            if (MOD(i,2).eq.1) then
                  print*, "odd"
                  else
                  print*, "even"
                  endif 
            if (MOD(i,7).eq.0) print*, "it's a multiple of seven"
            if ((sqrt(real(i))-NINT(sqrt(real(i)))).eq.0)
     >print*, "perfect square"
            if (i.lt.0) then
                  exit=.true.
                  print*, "exiting program..."
                  ENDIF
            if (i.eq.0) go to 9999
      end DO

c print end of program time
      call itime(finish)
      DO 9980 i=1,3
            write (6,*) ! write to screen 3 newline
            write(finishTime_str(3*i-2:3*i), "(I2.2,A)") finish(i),":" ! the last character will be discarded since it's out of range
 9980 end DO
      print*, "__Program finished running at __ ", finishTime_str
      DO 9990 i=1,6,1
            write(6,*) "|"
 9990 end DO
 9999 END PROGRAM

c Apparently array multiplication works!
c "INTEGER y(1:11)".eq."INTEGER, DIMENSION(0:11) :: y"

c Fortran doesn't support "x,y,z= 1,2,3" assignments like in Python
c but you are not allowed to declare MORE variables after assginments
c fortran is absolute crap at evaluating values in print statements.
c also it is crap at implicitly declaring arrays.

c rounding error is bound to occur when using Reals
c finishTime_str=trim(finishTime_str) !good practice to clean up the string

c Function->Implicit->Integer->Parameter->Other Variables->Calculation->print
c     /(i, i=min, max, step)/ ! is the pythogn equivalent of
c     [i for i in range(imin, imax, istep)]

c well "i" has to be an integer when using it to create arrays. This sucks.
c write (*,*) (with nothing behind it) produces a new line character to screen
c one can array to do the same thing all at once (convert into real, divide by a real, etc.)