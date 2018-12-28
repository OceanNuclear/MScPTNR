      REAL FUNCTION dummy(x)
      IMPLICIT DOUBLE PRECISION(A-Z)
      PARAMETER(PI=3.14159265, TAU=pi*2)
      dummy=1.00!must refer to itself(func_name)
      END
c end of functions
c start of main functions
      PROGRAM MAIN
      IMPLICIT NONE

c variables (unknown/known)
      CHARACTER*8 finishTime_str
      INTEGER finish(3),i
      READ(10,20)
 10   
c PARAMETERS (known)

c Actual calculations

c print program end time
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