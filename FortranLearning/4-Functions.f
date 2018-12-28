      SUBROUTINE orderWatch()
      IMPLICIT INTEGER(A-Z)
      DOUBLE PRECISION Price
      read(5,*) Quantity
c      IF ((Quantity.lt.0).and.(isnan(Quantity))) then
      IF (Quantity.lt.0)     then
            print*,"Please enter a proper quantity instead!"
            go to 10
      end IF
      IF (Quantity.eq.1)     then
            write(6,"(A,I3,A)") "Ordering 1 watch at the price of £15"
            go to 10
      end IF
      write(6,"(A,I3,A)") "Ordering", Quantity, " watches..."
      IF    (Quantity.lt.5 ) then
            Discount = 5
      elseif(Quantity.lt.10) then
            Discount = 10
      elseif(Quantity.lt.30) then
            Discount = 15
      elseif(Quantity.lt.100)then
            Discount = 20
      elseif(Quantity.lt.300)then
            Discount = 25
      else  
            Discount = 30
      end IF
      price= DBLE(Quantity*(100-Discount)/100.0 *15)
      write(6,"(A,I3,A)") "at the prce of £15 per watch with",Discount,
     >"% Discount,"
      write(6,"(A,F9.2)") "Total price = £", price
 10   END


      SUBROUTINE BindingEnergy(N,P)
      IMPLICIT INTEGER(A-Z)
      nOdd = mod(N,2)
      pOdd = mod(P,2)
      IF(nOdd+pOdd-1) 20, 30, 40
 20         print*, "+ve binding energy contribution"
            GO TO 50
 30         print*, "zero binding energy contribution"
            GO TO 50
 40         print*, "-ve binding energy contribution"
            GO TO 50
 50   CONTINUE
      END SUBROUTINE
c Function to be tested
      FUNCTION parseArray(x)
      IMPLICIT DOUBLE PRECISION(A-Z)
      INTEGER DIM
c      DOUBLE PRECISION parseArray(1:DIM)
      print*, x
      parseArray = x+0
      END
