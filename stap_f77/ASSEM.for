      SUBROUTINE ASSEM (AA)
C     调用ELEMENT子程序，装配整体刚度矩阵
      COMMON /EL/ IND,NPAR(10),NUMEG,MTOT,NFIRST,NLAST,ITWO
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT
      DIMENSION AA(1)
      
      REWIND IELMNT
      
      DO 200 N = 1, NUMEG
          READ(IELMNT) NUMEST,NPAR,(AA(I),I=1,NUMEST)
          CALL ELEMNT
  200 CONTINUE
      RETURN
      END