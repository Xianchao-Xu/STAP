      SUBROUTINE ADDBAN(A,MAXA,S,LM,ND)
C     装配单元刚度矩阵（上三角阵）到整体刚度矩阵中
C         A：整体刚度矩阵
C         S：单元刚度矩阵
C         ND：单元刚度矩阵中的自由度
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION A(1),MAXA(1),S(1),LM(1)
      
      NDI = 0
C     I：单元刚度矩阵各列的自由度编号
      DO 200 I = 1, ND
C         II: 单元各列的自由度在整体刚度矩阵中的编号，也就是全局自由度编号、方程的列号
          II = LM(I)
C         自由度为0时，该行（列）的元素不组装，直接进入下一行的对角线元素
C         
          IF (II) 200, 200, 100
C         MI：第II列对角线元素在刚度矩阵中的存储位置
  100     MI = MAXA(II)  
          KS = I
C         J：单元刚度矩阵各行的自由度编号
          DO 220 J = 1, ND
C             JJ：单元刚度矩阵各行的整体自由度编号
              JJ = LM(J)
              IF (JJ) 220, 220, 110
  110         IJ = II - JJ
C             IJ >= 0时，说明是刚度矩阵的上三角部分
              IF (IJ) 220, 210, 210
C             KK: 整体刚度矩阵元素的索引
C             KSS: 单元刚度矩阵元素的索引
  210         KK = MI + IJ
              KSS = KS
              IF (J. GE. I) KSS = J + NDI
              A(KK) = A(KK) + S(KSS)
  220     KS = KS + ND - J
  200 NDI = NDI + ND - I
      RETURN
      END