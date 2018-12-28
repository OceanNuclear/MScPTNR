      FUNCTION GravitationalPull(m1,m2,r) !m1, m2, r has to be in metric.
      IMPLICIT DOUBLE PRECISION(A-Z)
      Parameter(pi=3.1415926535, tau=6.283185307179586, G=6.672E-11)
      force = (G*m1*m2)/(r**2)
      print*, "the force of attraction =", force, "Newtons"
      END

      SUBROUTINE Gaussian0(mu, sigma, x)
      IMPLICIT DOUBLE PRECISION(A-Z)
c      DOUBLE PRECISION mu, sigma
      parameter(tau= 6.283185307179586)
      x = 1.00/sqrt(tau*(sigma**2)) * exp((x-mu)**2/(-2*sigma**2)) !Num of decimal place seems to = max(denom, numerator)
      END
c end of functions to be declared before program
      SUBROUTINE divideA(i,sideLen)
      IMPLICIT INTEGER (I)
      IMPLICIT DOUBLE PRECISION(A-H,J-Z)
      shortside = sideLen/2.
      sideLen=sideLen/sqrt(2.0)
      write(6,"(A,I1,A,F6.1,A,F6.1,A)") "A",i ," has side lengths",
     >sideLen, "cm  x", shortside, " cm"
      END