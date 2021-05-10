      SUBROUTINE ADDRES (MAXA,MHT)
C     计算一维变带宽存储的刚度矩阵对角线元素的位置
C         MHT: 列高
C         MAXA: 定位向量，对角元素位置
      COMMON /SOL/ NUMNP,NEQ,NWK,NUMEST,MIDEST,MAXEST,MK
      DIMENSION MAXA(*),MHT(*)
      
C     初始化定位向量
      NN = NEQ + 1
      DO 20 I = 1, NN
   20 MAXA(I) = 0.0
      
      MAXA(1) = 1
      MAXA(2) = 2
      MK = 0
      IF (NEQ .EQ. 1) GO TO 100
      DO 10 I = 2, NEQ
          IF (MHT(I) .GT. MK) MK = MHT(I)
   10 MAXA(I+1) = MAXA(I) + MHT(I) + 1
  100 MK = MK + 1
C     NWK: 一维数组中存储的刚度元素个数
      NWK = MAXA(NEQ+1) - MAXA(1)
      RETURN
      END