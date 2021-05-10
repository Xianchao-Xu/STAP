      SUBROUTINE INPUT (ID,X,Y,Z,NUMNP,NEQ)
C     读取、生成并输出节点数据
C     计算方程数，并将节点信息存入数组
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT
      DIMENSION X(1),Y(1),Z(1),ID(3,NUMNP)

      WRITE (IOUT, 2000)
      WRITE (IOUT, 2010)
      WRITE (IOUT, 2020)
      KNOLD=0
      NOLD=0

   10 READ (IIN, 1000) N,(ID(I,N),I=1,3),X(N),Y(N),Z(N),KN
      WRITE (IOUT,2030) N,(ID(I,N),I=1,3),X(N),Y(N),Z(N),KN
      IF (KNOLD .EQ. 0) GO TO 50
      NUM = (N-NOLD) / KNOLD
      NUMN = NUM - 1
      IF (NUMN .LT. 1) GO TO 50
      XNUM = NUM
      DX = (X(N) - X(NOLD)) / XNUM
      DY = (Y(N) - Y(NOLD)) / XNUM
      DZ = (Z(N) - Z(NOLD)) / XNUM
      K = NOLD
      DO 30 J = 1, NUMN
      KK = K
      K = K + KNOLD
      X(K) = X(KK) + DX
      Y(K) = Y(KK) + DY
      Z(K) = Z(KK) + DZ
      DO 30 I = 1, 3
      ID(I, K) = ID(I, KK)
   30 CONTINUE

   50 NOLD = N
      KNOLD =KN
      IF (N .NE. NUMNP) GO TO 10

      WRITE(IOUT, 2015)
      WRITE(IOUT, 2020)
      DO 200 N=1,NUMNP
  200 WRITE(IOUT, 2030) N,(ID(I,N),I=1,3),X(N),Y(N),Z(N),KN
      
      NEQ = 0
      DO 100 N = 1, NUMNP
      DO 100 I = 1, 3
c     110、120、110分别在条件小于0、等于0、大于0时执行
      IF (ID(I,N)) 110,120,110
  120 NEQ = NEQ + 1
      ID(I, N) = NEQ
      GO TO 100
  110 ID(I, N) = 0
  100 CONTINUE
      WRITE(IOUT,2040) (N,(ID(I,N),I=1,3),N=1,NUMNP)

      RETURN

 1000 FORMAT(4I5,3F10.0,I5)
 2000 FORMAT(//,' NODAL POINT DATA',/)
 2010 FORMAT(' INPUT NODAL DATA',//)
 2015 FORMAT(//,' GENERATED NODAL DATA',//)
 2020 FORMAT('  NODE',10X,'BOUNDARY',25X,'NODAL POINT',17X,'MESH',/,
     2' NUMBER     CONDITION  CODES',21X,'COORDINATES',14X,'GENERATING',
     2/,77X,'CODE',/,
     315X,'X    Y    Z',15X,'X',12X,'Y',12X,'Z',10X,'KN')
 2030 FORMAT(I5,6X,3I5,6X,3F13.3,3X,I6)
 2040 FORMAT(//,' EQUATION NUMBERS',//,'   NODE',9X,
     1 'DEGREES OF FREEDOM',/,'  NUMBER',//,
     2 '     N',13X,'X    Y    Z',/,(1X,I5,9X,3I5))

      END