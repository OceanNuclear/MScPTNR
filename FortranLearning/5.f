c Can I use small case for non-declaration statement please?
c Can I also do IMPLICIT DOUBLE PRECISION(A-Z)?
c Also can I use ::?
c and would you mark me down for inconsistent use of indents and capitalizations?
c and Random use of line numbers
	  INCLUDE "GeneralLibrary.f"
      REAL FUNCTION dummy(x)
      IMPLICIT DOUBLE PRECISION(A-Z)
      PARAMETER(PI=3.14159265, TAU=pi*2)
      dummy=1.00!must refer to itself(func_name) at the end
      END Function
      
      FUNCTION Quatdrature(array,numElem)
      IMPLICIT INTEGER(A-Z)
      DOUBLE PRECISION array(*)!Gives you the element i
      DOUBLE PRECISION Sum
      DOUBLE PRECISION Quatdrature
      Sum = 0	!double pre
      DO I=1, numElem !Int, int
        Sum = Sum + numElem**2
      end DO
      Quatdrature = sqrt(Sum) !double pre
      END Function

      SUBROUTINE MULTIPLY(elem, i,j )
      IMPLICIT INTEGER(A-Z)
      elem = i*j
      END SUBROUTINE
c end of functions

c start of main functions
      PROGRAM MAIN
      IMPLICIT DOUBLE PRECISION(A-Z)
      PARAMETER(pi=3.14159265, tau=pi*2) ! general purpose parameters
c variables
      CHARACTER*8 finishTime_str
      INTEGER finish(3),i,j, sideLeng
c PARAMETERS (known)
c DON'T TOUCH (to calculate linspace)
      PARAMETER(sideLeng=12)!implicitly double precisions
      INTEGER XarySF, XaryMi, XaryMa, XaryStep, XaryLeng
c Allocate memory for arrays and give values to x
      INTEGER table
      DIMENSION table(1:sideLeng, 1:sideLeng)

c Decide values for array variable

c Actual calculations
      DO i=1,sideLeng
      	DO j=1,sideLeng
      		CALL MULTIPLY(table(i,j),i,j)
      	end DO
      end DO

c print program end time
      CALL ProgEndTime()
      END PROGRAM