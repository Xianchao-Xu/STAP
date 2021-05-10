      SUBROUTINE WRITED(DISP, ID, NEQ, NUMNP)
C     打印位移数据
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT
      DIMENSION DISP(NEQ), ID(3,NUMNP)
      DIMENSION D(3)
      
      WRITE(IOUT, 2000)
      IC = 4
      
      DO 100 II = 1, NUMNP
          IC = IC + 1
          IF (IC .LT. 56) GO TO 105
          WRITE(IOUT, 2000)
          IC = 4
  105     DO 110 I = 1, 3
  110     D(I) = 0.
          DO 120 I = 1, 3
              KK = ID(I, II)
              IL = I
  120     IF (KK .NE. 0) D(IL) = DISP(KK)
  100 WRITE(IOUT, 2010) II, D
      
      RETURN
      
 2000 FORMAT(///,' D I S P L A C E M E N T S',//,'  NODE ',10X,
     1       'X-DISPLACEMENT    Y-DISPLACEMENT    Z-DISPLACEMENT')
 2010 FORMAT(1X,I3,8X,3E18.6)
      
      END