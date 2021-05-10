      SUBROUTINE COLHT (MHT, ND, LM)
C     计算列高（本程序中，列高不计算对角线元素）
C     列高等于i-mi，其中：
C         i是列号，也就是自由度编号
C         mi是第i列首个非零元素的行号，也就是单元中的最小自由度编号
      COMMON /SOL/ NUMNP,NEQ,NWK,NUMEST,MIDEST,MAXEST,MK
      DIMENSION LM(1),MHT(1)
      
C     循环完后，LS为单元中方程编号最小的自由度
      LS = 100000
      DO 100 I = 1, ND
          IF (LM(I)) 110, 100, 110
  110     IF (LM(I) - LS) 120, 100, 100
  120     LS = LM(I)
  100 CONTINUE
      
C     遍历获取列高
C     下面程序中，II为自由度编号，是第i个对角线元素
C     由于刚度矩阵为对称矩阵，在本程序中只存储上三角阵
C     所以，自由度编号大于II的会存在于下三角阵中，不需要考虑
      DO 200 I = 1, ND
          II = LM(I)
          IF (II .EQ. 0) GO TO 200
          ME = II - LS
          IF (ME .GT. MHT(II)) MHT(II) = ME
  200 CONTINUE
      RETURN
      END
