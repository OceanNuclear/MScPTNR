      PROGRAM EXER3
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      REAL PI, TAU
      PARAMETER (PI=3.14159265, TAU=PI*2)
      write (*,"A=", advance="no")
c      print *, "A="
c
      READ *, A
      PRINT *, "B="
      READ *, B
      C=B
      B=A
      B=C
      PRINT *, "A=", A
      PRINT *, "B=", B
      END
