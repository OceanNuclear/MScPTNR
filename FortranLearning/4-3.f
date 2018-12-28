      SUBROUTINE PaperMarks(mark, papernum)
      IMPLICIT DOUBLE PRECISION(A-Z)
      INTEGER papernum
 150  write(6,"(A,I1,A)") "Marks for paper ", papernum, " :"
      read(5, *) mark
      if (.not.((mark.le.100).or.(mark.ge.0))) then
            print*, "marks not in valid range! Please re-enter:" !catches errors
            go to 150
      end IF
      END SUBROUTINE

      SUBROUTINE DistPassFail(avg)
      IMPLICIT DOUBLE PRECISION(A-Z)
      IF (avg.GE.70) then
            print*, "Distinction"
      elseif (avg.GE.40) then
            print*, "Pass"
      else
            print*, "Fail"
      end IF
      END SUBROUTINE

      SUBROUTINE summation(numClass, sumClass, datum, lLim, uLim)
      IMPLICIT INTEGER(A-Z)
      DOUBLE PRECISION datum, sumClass
      IF ((datum.GE.lLim).and.(datum.LT.uLim)) then
            numClass = numClass + 1
            sumClass = sumClass + datum
      end IF
      END SUBROUTINE
c end of functions

      PROGRAM MAIN
      IMPLICIT INTEGER(A-Z)
c variables (unknown)
      CHARACTER*8 finishTime_str
      INTEGER finish(3),Input, I, J
c VARIABLES (known)
      INTEGER :: numDist = 0
      INTEGER :: numPass = 0
      INTEGER :: numFail = 0
      DOUBLE PRECISION :: sumDist = 0.0
      DOUBLE PRECISION :: sumPass = 0.0
      DOUBLE PRECISION :: sumFail = 0.0
c PARAMETERS (known)
      INTEGER, PARAMETER :: numPaper= 3
      INTEGER, PARAMETER :: minRef  = 101
      INTEGER, PARAMETER :: maxRef  = 199
c DON'T TOUCH (to initialize x)
c Allocate memory for arrays and give values to x
      DOUBLE PRECISION, dimension(minRef:maxRef,1:numPaper):: Student
      DOUBLE PRECISION, dimension(minRef:maxRef):: avg
      DATA ((Student(i,j),i=minRef,maxRef),j=1,numPaper) /297*-1/ !lay out the Student(:,:) array flat and add data to it
      avg = [(-1, i=minRef,maxRef)]
c Actual calculations
c      print*, Student
c      print*, shape([(([(-1, i=1,numPaper)]) , j =minRef,maxRef) ])
      DO    !labels; exit condition stated below
            print*, "enter student ID:"
            read(5,*) ID
            IF ((ID.GE.minRef).and.(ID.LE.maxRef)) then
                  avg(ID) = 0
                  DO i=1,numPaper
                        call PaperMarks(Student(ID,i), i)
                        avg(i)= avg(i)+ (Student(ID,i)/DBLE(numPaper))
                        print *, Student(ID,:)
                  end DO
            elseif (ID.eq.999) then
                  go to 100
            else
                  go to 9999
            end IF
      end DO
c starting tomputation
 100  DO I=minRef, maxRef
            call summation(numDist, sumDist, avg(i), 70, 100)
            call summation(numPass, sumPass, avg(i), 40, 70)
            call summation(numFail, sumFail, avg(i), 0, 40)
      end DO
      print*, avg
      write(6,"(A,F6.3)") "Average dist mark = ",(sumDist/DBLE(numDist))
      write(6,"(A,I3)")  "number of dist = ", numdist
      write(6,"(A,F6.3)") "Average pass mark = ",(sumPass/DBLE(numPass))
      write(6,"(A,I3)")  "number of pass = ", numpass
      write(6,"(A,F6.3)") "Average fail mark = ",(sumFail/DBLE(numFail))
      write(6,"(A,I3)")  "number of fail = ", numfail
c run through all avgs
c add the avgs to 

c program accounts for repeated entry as well
c      print*, "please enter their reference number:"
c            read() refNum(i)
c            if (refNum.eq.999) go to 9970


c print program end time
 9999 call itime(finish)
      DO i=1,3
            write (6,*) ! write to screen 3 newline
            write(finishTime_str(3*i-2:3*i), "(I2.2,A)") finish(i),":" ! the last character will be discarded since it's out of range
      end DO
      print*, "__Program finished running at __ ", finishTime_str
      DO i=1,6,1
            write(6,*) "|"
      end DO
      END PROGRAM