      PROGRAM EXER2_2
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      REAL PI, TAU
      PARAMETER (PI=3.14159265, TAU=PI*2, C=2.99792458E8)
      PRINT *, "please input the wavelength in nm:"
      READ *, wavelength
      wavelength = wavelength*10E-9
      PRINT *, "nm"
      f=c/wavelength
      PRINT *, "frequency in Hz = ", f
      END
