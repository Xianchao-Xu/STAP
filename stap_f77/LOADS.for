      SUBROUTINE LOADS(R,NOD,IDIRN,FLOAD,ID,NLOAD,NEQ)
C     读取节点载荷数据
C     计算不同工况的载荷向量R，并将R写入文件中
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /VAR/ NG, MODEX
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT
      DIMENSION R(NEQ),NOD(1),IDIRN(1),FLOAD(1)
      DIMENSION ID(3,1)
      
      WRITE(IOUT,2000)
      READ (IIN,1000) (NOD(I),IDIRN(I),FLOAD(I),I=1,NLOAD)
      WRITE(IOUT,2010) (NOD(I),IDIRN(I),FLOAD(I),I=1,NLOAD)
      IF (MODEX .EQ. 0) GO TO 900
      
      DO 210 I = 1,NEQ
  210 R(I) = 0.
      
      DO 220 L = 1, NLOAD
          LN = NOD(L)
          LI = IDIRN(L)
          II = ID(LI,LN)
          IF (II) 220,220,240
  240     R(II) = R(II) + FLOAD(L)
  220 CONTINUE
      
C     Fortran Unformatted Data
      WRITE(ILOAD) R
  200 CONTINUE
      
  900 RETURN
      
 1000 FORMAT (2I5,F10.0)
 2000 FORMAT (//,'    NODE        DIRECTION      LOAD',/,
     1        '   NUMBER', 20X,'MAGNITUDE')
 2010 FORMAT (' ',I6,9X,I4,7X,E12.5)
      END