      SUBROUTINE RUSS (ID,X,Y,Z,U,MHT,E,AREA,LM,XYZ,MATP)
C     桁架单元
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      REAL A
      COMMON /SOL/ NUMNP,NEQ,NWK,NUMEST,MIDEST,MAXEST,MK
      COMMON /DIM/ N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,N13,N14,N15
      COMMON /EL/ IND,NPAR(10),NUMEG,MTOT,NFIRST,NLAST,ITWO
      COMMON /VAR/ NG,MODEX
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT
      COMMON A(1)
      
      DIMENSION X(1),Y(1),Z(1),ID(3,1),E(1),AREA(1),LM(6,1),
     1          XYZ(6,1),MATP(1),U(1),MHT(1)
      DIMENSION S(21),ST(6),D(3)
      
      EQUIVALENCE (NPAR(1),NPAR1),(NPAR(2),NUME),(NPAR(3),NUMMAT)
      ND = 6
      GO TO (300, 610, 800), IND
      
C     *******************************************************************
C     读取和生成单元信息
C     *******************************************************************
  300 WRITE (IOUT, 2000) NPAR1, NUME
      
C     读取材料信息
      IF (NUMMAT .EQ. 0) NUMMAT = 1
      WRITE (IOUT, 2010) NUMMAT
      
      WRITE (IOUT, 2020)
      DO 10 I = 1, NUMMAT
          READ (IIN, 1000) N, E(N), AREA(N)
   10 WRITE (IOUT, 2030) N, E(N), AREA(N)
      
C     读取单元信息
      WRITE (IOUT, 2040)
      N = 1
C     桁架单元号，单元节点1，单元节点2，材料属性，自动生成参数
  100 READ (IIN, 1020) M, II, JJ, MTYP, KG
      IF (KG .EQ. 0) KG = 1
  120 IF (M .NE. N) GO TO 200
      I = II
      J = JJ
      MTYPE = MTYP
      KKK = KG
      
  200 XYZ(1, N) = X(I)
      XYZ(2, N) = Y(I)
      XYZ(3, N) = Z(I)
      
      XYZ(4, N) = X(J)
      XYZ(5, N) = Y(J)
      XYZ(6, N) = Z(J)

      MATP(N) = MTYPE
      
C     LM是自由度连接数组，数组中的元素为单元的自由度编号，也就是方程的行号
      DO 390 L = 1, 6
  390 LM(L, N) = 0
      DO 400 L = 1, 3
          LM(L, N) = ID(L, I)
  400 LM(L+3, N) = ID(L, J)
      
      CALL COLHT (MHT, ND, LM(1, N))
      
      WRITE (IOUT, 2050) N, I, J, MTYPE
      IF (N .EQ. NUME) GO TO 900
      N = N + 1
      I = I + KKK
      J = J + KKK
      IF (N .GT. M) GO TO 100
      GO TO 120
      
C     *******************************************************************
C     组装单元刚度矩阵（只存储上三角阵）
C     *******************************************************************
  610 DO 500 N = 1, NUME
          MTYPE = MATP(N)
C         XL2：单元长度的平方
          XL2 = 0.
          DO 505 L = 1, 3
              D(L)=XYZ(L,N)-XYZ(L+3,N)
  505     XL2 = XL2 + D(L) * D(L)
C         XL：单元长度L
          XL = SQRT(XL2)
C         XX：EA*L
          XX = E(MTYPE)*AREA(MTYPE)*XL
          
C         D(L)/XL为杆轴向与三个坐标轴的余弦
C         ST：杆轴向与三个坐标轴余弦 除以 单元长度L
          DO 510 L = 1, 3
              ST(L) = D(L)/XL2
  510     ST(L+3) = -ST(L)
          KL = 0
          DO 600 L = 1, 6
              YY = ST(L) * XX
              DO 600 K = L, 6
                  KL = KL + 1
  600     S(KL) = ST(K) * YY
          CALL ADDBAN(A(N3),A(N2),S,LM(1,N),ND)
  500 CONTINUE
      GO TO 900
      
  800 IPRINT = 0
      DO 830 N = 1, NUME
          IPRINT = IPRINT + 1
          IF (IPRINT .GT. 50) IPRINT = 1
          IF (IPRINT .EQ. 1) WRITE(IOUT, 2060) NG
          MTYPE = MATP(N)
          XL2 = 0.
          DO 820 L = 1, 3
              D(L) = XYZ(L, N) - XYZ(L+3, N)
  820     XL2 = XL2 + D(L)*D(L)
          
          DO 814 L = 1, 3
              ST(L) = (D(L) / XL2) * E(MTYPE)
  814     ST(L+3) = -ST(L)
          
          STR = 0.0
          DO 806 L = 1, 3
              I = LM(L, N)
              IF (I .LE. 0) GO TO 807
              STR = STR + ST(L) * U(I)
  807         J = LM(L+3, N)
              IF (J .LE. 0) GO TO 806
              STR = STR + ST(L+3) * U(J)
  806     CONTINUE
          P = STR * AREA(MTYPE)
          WRITE (IOUT, 2070) N, P, STR
          
  830 CONTINUE
      
  900 RETURN
      
 1000 FORMAT (I5, 2F10.0)
 1020 FORMAT (5I5)
 2000 FORMAT (' E L E M E N T   D E F I N I T I O N',///,
     1        ' ELEMENT TYPE ',13(' .'),'( NPAR(1) ) .. =',I5,/,
     2        '     EQ.1, TRUSS ELEMENTS',/,
     3        '     EQ.2, ELEMENTS CURRENTLY',/,
     4        '     EQ.3, NOT AVAILABLE',//,
     5        ' NUMBER OF ELEMENTS.',10(' .'),'( NPAR(2) ) .. =',I5,//)
 2010 FORMAT (' M A T E R I A L   D E F I N I T I O N',///,
     1        ' NUMBER OF DIFFERENT SETS OF MATERIAL',/,
     2        ' AND CROSS-SECTIONAL  CONSTANTS ',
     3                           4(' .'),'( NPAR(3) ) . . =',I5,//)
 2020 FORMAT ('  SET       YOUNG''S     CROSS-SECTIONAL',/,
     1        ' NUMBER     MODULUS',10X,'AREA',/,
     2        15x,'E',14X,'A')
 2030 FORMAT (/,I5,4X,ES12.5,2X,ES14.6)
 2040 FORMAT (//,' E L E M E N T   I N F O R M A T I O N',///
     1        ' ELEMENT     NODE     NODE       MATERIAL',/,
     2        ' NUMBER-N      I        J       SET NUMBER',/)
 2050 FORMAT (I5, 6X, I5, 4X, I5, 7X, I5)
 2060 FORMAT (///,' S T R E S S  C A L C U L A T I O N S  F O R  ',
     1        'E L E M E N T  G R O U P',I4,//,
     2        '  ELEMENT',13X,'FORCE',12X,'STRESS',/,
     3        '  NUMBER',/)
 2070 FORMAT (1X, I5, 11X, E13.6, 4X, E13.6)
      END