      SUBROUTINE ADDBAN(A,MAXA,S,LM,ND)
C     װ�䵥Ԫ�նȾ����������󣩵�����նȾ�����
C         A������նȾ���
C         S����Ԫ�նȾ���
C         ND����Ԫ�նȾ����е����ɶ�
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION A(1),MAXA(1),S(1),LM(1)
      
      NDI = 0
C     I����Ԫ�նȾ�����е����ɶȱ��
      DO 200 I = 1, ND
C         II: ��Ԫ���е����ɶ�������նȾ����еı�ţ�Ҳ����ȫ�����ɶȱ�š����̵��к�
          II = LM(I)
C         ���ɶ�Ϊ0ʱ�����У��У���Ԫ�ز���װ��ֱ�ӽ�����һ�еĶԽ���Ԫ��
C         
          IF (II) 200, 200, 100
C         MI����II�жԽ���Ԫ���ڸնȾ����еĴ洢λ��
  100     MI = MAXA(II)  
          KS = I
C         J����Ԫ�նȾ�����е����ɶȱ��
          DO 220 J = 1, ND
C             JJ����Ԫ�նȾ�����е��������ɶȱ��
              JJ = LM(J)
              IF (JJ) 220, 220, 110
  110         IJ = II - JJ
C             IJ >= 0ʱ��˵���ǸնȾ���������ǲ���
              IF (IJ) 220, 210, 210
C             KK: ����նȾ���Ԫ�ص�����
C             KSS: ��Ԫ�նȾ���Ԫ�ص�����
  210         KK = MI + IJ
              KSS = KS
              IF (J. GE. I) KSS = J + NDI
              A(KK) = A(KK) + S(KSS)
  220     KS = KS + ND - J
  200 NDI = NDI + ND - I
      RETURN
      END