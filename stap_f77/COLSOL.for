      SUBROUTINE COLSOL (A,V,MAXA,NN,NWK,NNM,KKK)
C     求解有限元的静力学平衡方程
C     输入参数：
C         A(NWK): 一维数组存储的整体刚度矩阵
C         V(NN): 载荷向量
C         MAXA(NNM): 刚度矩阵中的对角元素位置
C         NN: 方程总数
C         NWK: 刚度矩阵中存储的元素总数
C         NNM: 方程数加1
C         KKK: 标识符
C             1：三角分解
C             2：求解位移向量
C     输出参数：
C         A(NWK): 分解后的D和L
C         V(NN): 位移向量
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT
      DIMENSION A(NWK),V(1),MAXA(1)
      
      IF (KKK-2) 40, 150, 150
C     *******************************************************************
C     LDLT分解，获取L和D
C     *******************************************************************
C         N：列号
   40 DO 140 N = 1, NN
C         KN：第N列对角元素在数组A（刚度矩阵）中的位置
          KN = MAXA(N)
C         KL：第N列对角线元素上一行元素在A中的位置
          KL = KN + 1
C         KU：第N列首个非零元素在A中的位置
          KU = MAXA(N+1) - 1
C         KH：第N列去除对角线元素和第一个非零元素后剩余的元素数量
          kH = KU - KL
C         如果KH小于0，说明该列只有对角线元素
C         如果KH等于0，说明该列只有对角线元素以及对角线上面的那个元素
          IF (KH) 110, 90, 50
C         接下来的代码计算Finite Element Procedures中公式8.21中的临时变量g
C         从公式可知，各列首个非零元素所在的位置，g和k是相等的，不用处理，
C         所以，迭代从首个非零元素的下一行开始，到对角线为止（不包含对角线元素）
C         K：行号（不包括首个非零元素以及对角线元素）
   50     K = N - KH
          IC = 0
          KLT = KU
          DO 80 J = 1, KH
              IC = IC + 1
C             KLT：第N列第K行元素在数组A中的位置
              KLT = KLT - 1
C             KI：第K行对角线元素在数组A中的位置
              KI = MAXA(K)
C             ND：第N列第K行元素与第N列首个非零元素在A数组中存储位置的间隔
              ND = MAXA(K+1) - KI - 1
              IF (ND) 80, 80, 60
   60         KK = MIN0(IC, ND)
              C = 0.
              DO 70 L = 1, KK
   70         C = C + A(KI+L) * A(KLT+L)
              A(KLT) = A(KLT) - C
   80     K = K + 1
   90     K = N
          B = 0.
C         下面的循环是教材Finite Element Procedures中公式8.22、8.23的实现
          DO 100 KK = KL, KU
              K = K - 1
              KI = MAXA(K)
              C = A(KK) / A(KI)
              B = B + C * A(KK)
C         A(KK)：LDLT分解后LT矩阵中第N列第K行的元素，即教材Finite Element Procedures中公式8.22
  100     A(KK) = C
C         A(KN)：LDLT分解后对角阵D中第N个对角元素，即教材Finite Element Procedures中公式8.23
          A(KN) = A(KN) - B
C         如果对角线元素小于等于0，说明刚度矩阵不是正定矩阵，显示错误信息，退出程序
  110     IF (A(KN)) 120, 120, 140
  120     WRITE(IOUT, 2000) N, A(KN)
          GO TO 800
  140 CONTINUE
      GO TO 900

C     *******************************************************************
C     计算右手侧载荷向量
C     *******************************************************************
C     Finite Element Procedures公式8.24
  150 DO 180 N = 1, NN
          KL = MAXA(N) + 1
          KU = MAXA(N+1) - 1
          IF (KU - KL) 180, 160, 160
  160     K = N
          C = 0.
          DO 170 KK = KL, KU
              K = K - 1
  170     C = C + A(KK) * V(K)
          V(N) = V(N) - C
  180 CONTINUE
      
C     *******************************************************************
C     回代
C     *******************************************************************
C     Finite Element Procedures公式8.25
      DO 200 N = 1, NN
          K = MAXA(N)
  200 V(N) = V(N) / A(K)
      IF (NN .EQ. 1) GO TO 900
      N = NN
      DO 230 L = 2, NN
          KL = MAXA(N) + 1
          KU = MAXA(N+1) - 1
          IF (KU - KL) 230, 210, 210
  210     K = N
          DO 220 KK = KL, KU
              K = K - 1
  220     V(K) = V(K) - A(KK)*V(N)
  230 N = N - 1
      GO TO 900
      
  800 STOP
  900 RETURN

 2000 FORMAT(//' STOP - STIFFINESS MATRIX NOT POSITIVE DEFINITE',//,
     1         ' NONPOSITIVE PIVOT FOR EQUATION ',I8,//,
     2         ' PIVOT = ',E20.12)
      END