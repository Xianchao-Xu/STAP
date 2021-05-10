      SUBROUTINE COLSOL (A,V,MAXA,NN,NWK,NNM,KKK)
C     �������Ԫ�ľ���ѧƽ�ⷽ��
C     ���������
C         A(NWK): һά����洢������նȾ���
C         V(NN): �غ�����
C         MAXA(NNM): �նȾ����еĶԽ�Ԫ��λ��
C         NN: ��������
C         NWK: �նȾ����д洢��Ԫ������
C         NNM: ��������1
C         KKK: ��ʶ��
C             1�����Ƿֽ�
C             2�����λ������
C     ���������
C         A(NWK): �ֽ���D��L
C         V(NN): λ������
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT
      DIMENSION A(NWK),V(1),MAXA(1)
      
      IF (KKK-2) 40, 150, 150
C     *******************************************************************
C     LDLT�ֽ⣬��ȡL��D
C     *******************************************************************
C         N���к�
   40 DO 140 N = 1, NN
C         KN����N�жԽ�Ԫ��������A���նȾ����е�λ��
          KN = MAXA(N)
C         KL����N�жԽ���Ԫ����һ��Ԫ����A�е�λ��
          KL = KN + 1
C         KU����N���׸�����Ԫ����A�е�λ��
          KU = MAXA(N+1) - 1
C         KH����N��ȥ���Խ���Ԫ�غ͵�һ������Ԫ�غ�ʣ���Ԫ������
          kH = KU - KL
C         ���KHС��0��˵������ֻ�жԽ���Ԫ��
C         ���KH����0��˵������ֻ�жԽ���Ԫ���Լ��Խ���������Ǹ�Ԫ��
          IF (KH) 110, 90, 50
C         �������Ĵ������Finite Element Procedures�й�ʽ8.21�е���ʱ����g
C         �ӹ�ʽ��֪�������׸�����Ԫ�����ڵ�λ�ã�g��k����ȵģ����ô���
C         ���ԣ��������׸�����Ԫ�ص���һ�п�ʼ�����Խ���Ϊֹ���������Խ���Ԫ�أ�
C         K���кţ��������׸�����Ԫ���Լ��Խ���Ԫ�أ�
   50     K = N - KH
          IC = 0
          KLT = KU
          DO 80 J = 1, KH
              IC = IC + 1
C             KLT����N�е�K��Ԫ��������A�е�λ��
              KLT = KLT - 1
C             KI����K�жԽ���Ԫ��������A�е�λ��
              KI = MAXA(K)
C             ND����N�е�K��Ԫ�����N���׸�����Ԫ����A�����д洢λ�õļ��
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
C         �����ѭ���ǽ̲�Finite Element Procedures�й�ʽ8.22��8.23��ʵ��
          DO 100 KK = KL, KU
              K = K - 1
              KI = MAXA(K)
              C = A(KK) / A(KI)
              B = B + C * A(KK)
C         A(KK)��LDLT�ֽ��LT�����е�N�е�K�е�Ԫ�أ����̲�Finite Element Procedures�й�ʽ8.22
  100     A(KK) = C
C         A(KN)��LDLT�ֽ��Խ���D�е�N���Խ�Ԫ�أ����̲�Finite Element Procedures�й�ʽ8.23
          A(KN) = A(KN) - B
C         ����Խ���Ԫ��С�ڵ���0��˵���նȾ���������������ʾ������Ϣ���˳�����
  110     IF (A(KN)) 120, 120, 140
  120     WRITE(IOUT, 2000) N, A(KN)
          GO TO 800
  140 CONTINUE
      GO TO 900

C     *******************************************************************
C     �������ֲ��غ�����
C     *******************************************************************
C     Finite Element Procedures��ʽ8.24
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
C     �ش�
C     *******************************************************************
C     Finite Element Procedures��ʽ8.25
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