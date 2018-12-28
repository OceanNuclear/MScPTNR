      PROGRAM EXER2
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      REAL PI, TAU
      PARAMETER (PI=3.14159265, TAU=PI*2)
      NumPicker=4
      OwnersShare=0.4
      NumApple=INT(285)
      
      PRINT *, "The owner gets ", NINT(OwnersShare*NumApple)
      Remainder = NumApple-NINT(OwnersShare*NumApple)
      PRINT *, "Each picker gets", NINT(Remainder/4)
      PRINT *, MOD(NINT(Remainder),NumPicker), "apples were into a pie"
      END
