      PROGRAM DUMMYNAME
      IMPLICIT DOUBLE PRECISION(A-Z)
      REAL PI, TAU, a
      INTEGER R
      PARAMETER (PI=3.14159265, TAU=PI*2, a=9.81)
      R=1
      PRINT *, "initial upwards velocity in ms^-1="
      READ *, v_i
      PRINT *, "time in seconds="
      READ *, t
      s = -0.5*a*t*t+v_i*t
      PRINT*, "assuming earth's gravity, displacement after that time= "
     >, s
      END