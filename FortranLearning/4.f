      INCLUDE "4-Functions.f"
      PROGRAM EXERCISE4
      IMPLICIT DOUBLE PRECISION(A-Z)
      PARAMETER(pi=3.14159265, tau=pi*2) ! general purpose parameters
c Integers are only to be used when making loops; DBLE for more serious precision

c variables (unknown)
      CHARACTER*8 finishTime_str
      INTEGER finish(3),i
c VARIABLES (known)
      INTEGER :: N = 5
      INTEGER :: P = 2
      LOGICAL :: exit = .FALSE.
      DOUBLE PRECISION :: limit = 2.5
c PARAMETERS (known)
      INTEGER, PARAMETER :: numPaper = 3
c DON'T TOUCH (to calculate the integer variables)

c Allocate memory for arrays and give values to x
      DOUBLE PRECISION, dimension(1:6,1:5)    :: xnew
      INTEGER,          dimension(1:numPaper) :: marks

c main part of calculation
c part 1
      call orderWatch()
      print*,
c part 2
 10   I = 0
      DO
            I=I+1
            lognum=log(DBLE(i))
            write(6,"(A,I3,A,F8.2)") "log of ",I,"=",lognum
            if (lognum.gt.limit) go to 30 !exit condition
      end DO
  30  write(6,"(A, F6.4,A,I3)") "largest integer with log(number)<",
     >limit,"=",(I-1)
      write(6,*) "Enter a number as the upper limit:"
      read(5,*) limit
      IF ((limit.le.0.1).or.(limit.gt.2.5)) then
            print*, "number is out too large; exiting program"
            go to 9999
      end IF
      go to 10
c part 3
 

c print end of program time
 9999 call itime(finish)
      DO i=1,3
            write (6,*) ! write to screen 3 newline
            write(finishTime_str(3*i-2:3*i), "(I2.2,A)") finish(i),":" ! the last character will be discarded since it's out of range
      end DO
      print*, "__Program finished running at __ ", finishTime_str
      DO i=1,6
            write(6,*) "|"
      end DO
      END PROGRAM