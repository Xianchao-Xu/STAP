      SUBROUTINE ELCAL
C     遍历单元组，读取、生成并存储单元信息
      COMMON /SOL/ NUMNP,NEQ,NWK,NUMEST,MIDEST,MAXEST,MK
      COMMON /EL/ IND,NPAR(10),NUMEG,MTOT,NFIRST,NLAST,ITWO
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT
      COMMON A(1)
      
      REWIND IELMNT
      WRITE (IOUT,2000)
      
      DO 100 N = 1, NUMEG
          IF (N .NE. 1) WRITE (IOUT, 2010)
          
          READ (IIN, 1000) NPAR
          
          CALL ELEMNT
          
          IF (MIDEST .GE. MAXEST) MAXEST = MIDEST
          
          WRITE(IELMNT) MIDEST,NPAR,(A(I),I=NFIRST,NLAST)
  100 CONTINUE
      RETURN
      
 1000 FORMAT(10I5)
 2000 FORMAT(//,' E L E M E N T   G R O U P   D A T A',//)
 2010 FORMAT(' ')
      END