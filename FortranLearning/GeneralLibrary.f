      SUBROUTINE ProgEndTime()
      CHARACTER*8 finishTime_str
      INTEGER finish(3),i
      call itime(finish)
      DO i=1,3
            write (6,*) ! write to screen 3 newline
            write(finishTime_str(3*i-2:3*i), "(I2.2,A)") finish(i),":" ! the last character will be discarded since it's out of range
      end DO
      print*, "__Program finished running at __ ", finishTime_str
      DO i=1,6
            write(6,*) "|"
      end DO
      END SUBROUTINE