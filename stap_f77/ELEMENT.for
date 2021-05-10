      SUBROUTINE ELEMNT
C     调用不同的单元子程序
      COMMON /EL/ IND,NPAR(10),NUMEG,MTOT,NFIRST,NLAST,ITWO
      
      NPAR1 = NPAR(1)
      GO TO (1, 2, 3), NPAR1
C     1为桁架单元
    1 CALL TRUSS
      GO TO 900
    2 GO TO 900
    3 GO TO 900
  900 RETURN
      END